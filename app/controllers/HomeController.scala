package controllers

import javax.inject._

import com.eclipsesource.schema.{SchemaType, SchemaValidator}
import com.redis.RedisClient
import play.api.libs.json._
import play.api.mvc._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
// TODO - implement dependency injection. move Redis interaction to dependency.
// TODO - configure db url via configuration file
// TODO - explore and use SCAN for Redis retrieval. Not a good idea.
// TODO - validate based on 'schema' defined in the incoming json. lookup schema defined and saved in Redis via the API.

@Singleton
class HomeController @Inject() extends Controller {

  val redis = new RedisClient("localhost", 6379)

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

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

  // TODO - complete it!
  def getPlanDetails(id: Long) = Action { request =>

    Ok("getPlanDetails")
  }


  def createPlan = Action(parse.json) { request =>
    validator.validate(schemaForPlan, request.body).fold(
      invalid = { errors =>  BadRequest(errors.toString())},
      valid = { validJson =>
        validJson match {
          case q:JsObject =>
            val g: String = s"${(q.value("schema").as[JsString]).value}_${(q.value("id").as[JsString]).value}"
            redis.set(g, validJson.toString()) match {
            case false => FailedDependency
            case _ => Created("Plan created")
          }
          case _ => BadRequest("String could not be parsed as a Json Object")
        }
      }
    )
  }



  // TODO - Deal with nested structure updates
  def updatePlan = Action(parse.json) { request =>

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
                    validator.validate(schemaForPlan, finalJson).fold(
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



  // Recursively find Object types and use the latest value for scalar items (Strings)
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

  val validator = new SchemaValidator()

  val schemaForPlan = Json.fromJson[SchemaType](Json.parse(
    """{
      |"$schemaForPlan": "http://json-schema.org/draft-04/schema#",
      |  "title": "Plan",
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
      |      "id",
      |      "planCategory",
      |      "planShortName",
      |      "parentsCovered",
      |      "schema",
      |      "planCost"
      |    ]
    }""".stripMargin)).get
}
