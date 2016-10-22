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

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */

//TODO - 1. The Supported Operations Should Include get, post , put, merge, and delete.

// DONE  - TODO - 2 .If you have implemented the crud operations by specifying different URIs for post, get, put, patch, and delete then you also need to rework the URI signatures. The URI signature should be of the following forms:
///{plans}
///{plans}/{id}
//Note how we do not hardcode the type of the object

// DONE - TODO - 3. If you do not return the UUID on a post, you need to change your implementation to do just that.

//TODO - 4. The way to uniquely identify an object is through the use of _id or id and not using planId. The latter won't scale when your system needs to support multiple or many objects.
// All nested objects must also have an id.


//TODO - 5. If you have not demonstrated the use of the Etag or if match headers, you should also do that
// TODO - 6. Ideally, the schema should not be read from the file system but rather from the data store

// TODO - 7. Security - Bearer Token

// TODO -   implement dependency injection. move Redis interaction to dependency.
// TODO -  configure db url via configuration file


@Singleton
class HomeController @Inject() (cache: CacheApi) extends Controller {

  val validator = new SchemaValidator()
  val redis = new RedisClient("localhost", 6379)

  def getAllPlans = Action {
    val planKeys = redis.keys("plan_*")
    planKeys match {
      case None => NoContent
      case Some(gos) => gos.isEmpty match {
        case true => NoContent
        case false => Ok {
          val gos1 = for (go <- gos; g <- go) yield redis.get (g)
          val qs = for (go1 <- gos1; g1 <- go1) yield Json.parse (g1)
          JsArray (qs)
        }
      }
    }
  }

  // This function allows GET semantics on all content schema types, including the schema themselves
  def get(schema:String, id: String) = Action { request =>


    // 1. compare cached etag and request headers. If match found, that would be the fastest 204.

    // 2. if etag not found in cache, but is present in headers. fetch item from redis, generate hash and compare.
    // Still a chance for saving bandwidth by returning just 204 and no data.

    // 3. last resort is: return full data with 200 and etag value for future use.




    val jsonData = redis.get(s"${schema}_$id")
    jsonData match {
      case None => NoContent
      case Some(g) =>
        // find Etag in cache. If not found, generate it using the hash of the contents
        val etag: String = cache.getOrElse[String](s"etag_${schema}_$id") {
          val sha: MessageDigest = MessageDigest.getInstance("SHA-1")
          val keyBytes = sha.digest(g.toCharArray.map(_.toByte))
          val etagStr = keyBytes.toString()
          cache.set(s"etag_${schema}_$id", etagStr)
          etagStr
        }


        println(s"form headers: ${request.headers.toSimpleMap("If-None-Match")}")
        println(s"from cache/generated: $etag")

        if(request.headers.toSimpleMap("If-None-Match").equals(etag)) {
          NotModified
        } else {
          Ok(g).withHeaders(ETAG -> etag)
        }
    }
  }


  // This function allows POST semantics on all content schema types, including the schema themselves
  // CONSIDER - moving schema creation to another method, say createSchema to neat separation for authurization later on.
  def create(schema: String) = Action(parse.json) { request =>

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
           val schemaType = Json.fromJson[SchemaType](Json.parse(j)) .get
           validator.validate(schemaType, reqBody).fold(
             invalid = { errors =>  BadRequest(errors.toString())},
             valid = { validJson =>
               validJson match {
                 case q:JsObject =>
                   // if id is not found, create a new UUID
                   val id:String =  q.value.get("id") match {
                     case Some(j:JsString) => j.value match {
                       case x:String if x.length >= 32 => x
                       case _ => UUID.randomUUID().toString
                     }
                     case _ => UUID.randomUUID().toString
                   }
                   val g: String = s"${(q.value("schema").as[JsString]).value}_$id"
                   val dataMap = mutable.Map.empty[String, JsValue]
                   dataMap ++= q.value
                   dataMap.put("id", JsString(id))
                   redis.set(g, JsObject(dataMap).toString()) match {
                     case false => FailedDependency
                     case _ => Created(id)
                   }
                 case _ => BadRequest("String could not be parsed as a Json Object")
               }
             }
           )
       }
     }
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
                                  case _ => Ok("merge/patch successful")
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

  // TODO 1 - enforce id for object updates
  // TODO 2 - complete for arrays of objects
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
    case _ => newVal
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  val schemaForPlan = Json.fromJson[SchemaType](Json.parse(
    """{
      |"$schemaForPlan": "http://json-schema.org/draft-04/schema#",
      |  "title": "plan",
      | "description": "A plan object encapsulates cost and benefits of the health plan",
      | "type": "object",
      |"properties": {
      |   "id" : { "type": "string" },
      |   "schema" : { "type": "string" },
      |  "planCategory": { "type": "string" },
      |  "planShortName": { "type": "string" },
      |  "parentsCovered":  { "type": "boolean" },
      |  "planCost" : {"type": "object", "properties" : {
      |                                     "copay": {"type":"number"},
      |                                     "premium": {"type":"number"}
      |                                }
      |             },
      |  "servicesSupported": { "type": "array", "items" :{"type":"string"}
      |               }
      |},
      |    "required": [
      |      "planCategory",
      |      "planShortName",
      |      "parentsCovered",
      |      "schema",
      |      "planCost"
      |    ]
    }""".stripMargin)).get
}
