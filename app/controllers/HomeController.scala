package controllers

import java.security.MessageDigest
import java.util.UUID
import javax.inject.{Inject, _}

import com.eclipsesource.schema.{SchemaType, SchemaValidator}
import com.redis.RedisClient
import play.api.cache.CacheApi
import play.api.libs.json.{JsObject, _}
import play.api.mvc._

import scala.collection.mutable

// TODO -  configure db url via configuration file

@Singleton
class HomeController @Inject() (cache: CacheApi, redis: RedisClient, validator: SchemaValidator) extends Controller {

  def headers = List(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST, OPTIONS, DELETE, PUT",
    "Access-Control-Max-Age" -> "3600",
    "Access-Control-Allow-Headers" -> "Origin, Content-Type, Accept, Authorization",
    "Access-Control-Allow-Credentials" -> "true"
  )

  def options(schema:String, id: String)  = Action { request =>
    NoContent.withHeaders(headers : _*)
  }

  // This function allows GET semantics on all content schema types, including the schema themselves
  def get(schema:String, id: String) = Action { request =>

    // CONSIDER - changing as per this for best performance:
    // 1. compare cached etag and request headers. If match found, that would be the fastest 204.
    // 2. if etag not found in cache, but is present in headers. fetch item from redis, generate hash and compare.
    // Still a chance for saving bandwidth by returning just 204 and no data.
    // 3. last resort is: return full data with 200 and etag value for future use.

    val jsonData = redis.get(s"${schema}_$id")
    jsonData match {
      case None => NotFound
      case Some(g) =>

        val etag: String = cache.getOrElse[String](s"etag_${schema}_$id") {
          val startTime = System.currentTimeMillis()
          val sha: MessageDigest = MessageDigest.getInstance("SHA-1")
          val keyBytes = sha.digest(g.toCharArray.map(_.toByte))
          val etagStr = keyBytes.toString()
          println(s"time for hashing: ${System.currentTimeMillis() - startTime}")
          cache.set(s"etag_${schema}_$id", etagStr)
          etagStr
        }
        request.headers.toSimpleMap.get("If-None-Match") match {
          case Some(g1) if g1 == etag => NotModified
          case _ => Ok(g).withHeaders(ETAG -> etag, ACCESS_CONTROL_ALLOW_ORIGIN -> "*")
        }
    }
  }

  // This function allows DELETE semantics on all content schema types, including the schema themselves
  def delete(schema:String, id: String) = Action { request =>
    val delCount = redis.del(s"${schema}_$id")
    delCount match {
      case None => NotFound
      case Some(l) => Ok(s"$l record(s) deleted")
    }
  }


  // This function allows POST semantics on all content schema types, including the schema themselves
  def create(schema: String) = Action(parse.json) { request =>

    val reqBody = request.body

    // CONSIDER After Everything Else Is Done- validating schema itself from json-schema.org specifications
     if (schema.equalsIgnoreCase("schema")) {
       (reqBody \ "title" ).validate[String] asOpt match {
         case Some(title) =>
           redis.set(s"schema_${title}", reqBody.toString()) match {
            case false => FailedDependency
            case _ => Created(s"schema_${title.toString()}")
         }
         case _ => BadRequest("Title not found in Schema")
       }
     } else {
       val jo = redis.get(s"schema_$schema")
       jo match {
         case None => BadRequest("Schema not found")
         case Some(j) =>
           val schemaType = Json.fromJson[SchemaType](Json.parse(j)) .get
           validator.validate(schemaType, reqBody).fold(
             invalid = { errors =>  BadRequest(errors.toString())},
             valid = { validJson =>
               validJson match {
                 case q:JsObject =>
                   // if id is not found, create a new UUID
                   val id: String = addIdToStructure(q)
                   val g: String = s"${(q.value("schema").as[JsString]).value}_$id"
                   val dataMap = mutable.Map.empty[String, JsValue]
                   dataMap ++= q.value
                   dataMap.put("id", JsString(id))
                   val jsonStr = JsObject(dataMap).toString()
                   redis.set(g, jsonStr) match {
                     case false => FailedDependency
                     case _ =>
                       pushToIndexer(jsonStr)
                       Created(id)

                   }
                 case _ => BadRequest("String could not be parsed as a Json Object")
               }
             }
           )
       }
     }
  }


  def pushToIndexer(jsonString: String) = redis.lpush("indexer", jsonString)

  // This function allows PUT semantics on all content schema types, including the schema themselves
  def put(schema: String, id: String) = Action(parse.json) { request =>

    val reqBody = request.body
    if (schema.equalsIgnoreCase("schema")) {
      (reqBody \ "title" ).validate[String] asOpt match {
        case Some(title) =>
          redis.set(s"schema_${title}", reqBody.toString()) match {
            case false => FailedDependency
            case _ => Created(s"schema_${title.toString()}")
          }
        case _ => BadRequest("Title not found in Schema")
      }
    } else {
      val jo = redis.get(s"schema_$schema")
      jo match {
        case None => BadRequest("Schema not found")
        case Some(j) =>
          val schemaType = Json.fromJson[SchemaType](Json.parse(j)).get
          validator.validate(schemaType, reqBody).fold(
            invalid = { errors =>  BadRequest(errors.toString())},
            valid = { validJson =>
              validJson match {
                case q:JsObject =>
                  val g: String = s"${(q.value("schema").as[JsString]).value}_$id"
                  val jsonString = JsObject(q.value).toString()
                  redis.set(g, jsonString) match {
                    case false => FailedDependency
                    case _ =>
                      pushToIndexer(jsonString)
                      Ok(id)
                    // CONSIDER - returning 200 for update, 201 for new creation
                  }
                case _ => BadRequest("String could not be parsed as a Json Object")
              }
            }
          )
      }
    }
  }


  def addIdToStructure(q: JsObject): String = {
    val id: String = q.value.get("id") match {
      case Some(j: JsString) => j.value match {
        case x: String if x.length >= 32 => x
        case _ => UUID.randomUUID().toString
      }
      case _ => UUID.randomUUID().toString
    }
    id
  }

  // This function allows PATCH semantics on all content schema types
  // Not allowing merge on Schema objects since there is no validation on Schema objects. Rather use PUT semantics for that.
  def mergeUpdate(schema: String) = Action(parse.json) { request =>
    val jo = redis.get(s"schema_$schema")
    jo match {
      case None => BadRequest("Schema not found")
      case Some(j) =>
        val schemaType = Json.fromJson[SchemaType](Json.parse(j)).get
        val reqJson =  request.body
        val gq: JsResult[String] = (reqJson \ "schema").validate[String]
        gq match {
          case e: JsError => println("Errors: " + JsError.toJson(e).toString()); BadRequest("schema is mandatory")
          case s: JsSuccess[String] =>
            val gq1: JsResult[String] = (reqJson \ "id").validate[String]
            gq1 match {
              case e: JsError => println("Errors: " + JsError.toJson(e).toString()); BadRequest("id is mandatory")
              case s1: JsSuccess[String] =>
                val g = s"${s.value}_${s1.value}"
                redis.get(g) match {
                  case None => BadRequest(s"${s.value} with id ${s1.value} not found")
                  case Some(json) =>
                    Json.parse(json) match {
                      case q: JsObject =>
                        val finalJson = merge(reqJson, q)
                        validator.validate(schemaType, finalJson).fold(
                          invalid = { errors =>  BadRequest(s"merged json did not pass the schema validation: ${errors.toString()}")},
                          valid = { validJson =>
                            validJson match {
                              case q:JsObject =>
                                val g = s"${(q.value("schema").as[JsString]).value}_${(q.value("id").as[JsString]).value}"
                                redis.set(g, validJson.toString()) match {
                                  case false => println("Errors: merged json could not be saved" ); FailedDependency
                                  case _ =>
                                    // TODO - test this
                                    val sha: MessageDigest = MessageDigest.getInstance("SHA-1")
                                    val keyBytes = sha.digest(g.toCharArray.map(_.toByte))
                                    val etagStr = keyBytes.toString()
                                    cache.set(s"etag_${schema}_${s1.value}", etagStr)
                                    pushToIndexer(validJson.toString())
                                    Ok("merge/patch successful").withHeaders(ETAG -> etagStr)
                                }
                              case _ => FailedDependency("Merged map could not be parsed as a Json Object")
                            }
                          }
                        )
                      case  _ => println("Errors: json from db could not be parsed" ); BadRequest("Bad data in database. could not be parsed to json")
                    }
                }
            }
        }
    }
  }



  // Recursively find Object types and use the latest value for scalar items (Strings)
  // CONSIDER - implementing using the approach of two maps - 1 for objects and 1 more for relationships
  def merge(newVal: JsValue, oldVal: JsValue) : JsValue = newVal match {
    case g:JsString => g
    case qNew:JsObject => oldVal match {
      case qOld:JsObject =>
        val mNew = qNew.value
        val mOld = qOld.value
        val commonKeys = mNew.keySet.intersect(mOld.keySet)
        val commonValuesMap = commonKeys.map((key) => (key, merge(mNew(key), mOld(key)))).toMap
        val mMerged = (mNew -- commonKeys) ++ (mOld -- commonKeys) ++ commonValuesMap
        JsObject(mMerged)
      case _ => qNew
    }
    case qArrNew:JsArray =>
      oldVal match {
        case qArrOld:JsArray =>
          qArrNew.value.head match {
            case x:JsObject =>
              val matchedObjects: Seq[(JsObject, JsObject)] = findCommonById(qArrNew, qArrOld)
              val uncommonNew = qArrNew.value.diff(matchedObjects.map(pair => pair._1))
              val uncommonOld = qArrOld.value.diff(matchedObjects.map(pair => pair._2))
              JsArray(uncommonNew ++ uncommonOld ++ matchedObjects.map(pair => merge(pair._1, pair._2)))
            case _ => qArrNew
        }
        case _ => qArrNew
      }
    case _ => newVal  // TODO - test what could come here and see if it needs to shout an error
  }

  def findCommonById(qArrNew: JsArray, qArrOld: JsArray): Seq[(JsObject, JsObject)] = {
    val matchedObjects: Seq[(JsObject, JsObject)] = for (
      n <- qArrNew.value;
      o <- qArrOld.value;
      nObj <- n.asOpt[JsObject];
      oObj <- o.asOpt[JsObject];
      if nObj \ "id" == oObj \ "id"
    ) yield (nObj, oObj)
    matchedObjects
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}
