package in.flow.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import in.flow.commformats._
import in.flow.users.UserAccount
import in.flow.{FlowError, WithErrorFlow}
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val flowResp = jsonFormat3(FlowResponse.apply)

  implicit val regReq = jsonFormat2(RegistrationRequest)
  implicit val regResp = jsonFormat1(RegistrationResponse)

  implicit def userRep = jsonFormat2(UserRepresentation)
  //  implicit def connLevel = iterableFormat[UserRepresentation]
  implicit def connResp = jsonFormat(ConnectionsResponse, "friends", "fof")

  implicit def toFlowResponse[T](res: WithErrorFlow[T])(implicit t: (T) => Option[JsValue]): FlowResponse = {
    res fold(
      fail => FlowResponse(None, Option(fail.errorCode), Option(fail.message)),
      s => FlowResponse(t(s))
    )
  }

  def toFlowResponse[T](what: Option[T])(implicit t: (T) => JsValue): FlowResponse = {
    FlowResponse(what map { w => t(w) })
  }

  protected def connectionsToFlowResponse(cons_f: Future[Seq[Set[UserAccount]]]): Future[FlowResponse] = {
    cons_f map { cons =>
      val fof = cons.tail map connectionsToRepresentations
      val friends = connectionsToRepresentations(cons.head)
      val r = ConnectionsResponse(friends, fof)
      val json: JsValue = r.toJson
      FlowResponse.success(json)
    } recover {
      case _: NoSuchElementException =>
        val emptyCons = ConnectionsResponse(Seq(), Seq())
        FlowResponse.success(connResp write emptyCons)
    }
  }

  private def connectionsToRepresentations(cons: Iterable[UserAccount]): Iterable[UserRepresentation] = cons map {
    c => UserRepresentation(c.user_id, c.display_name)
  }

  /* user id functions */
  protected def userIdToRegResp(uid: String): Option[JsValue] = Option(regResp write RegistrationResponse(uid))

  protected implicit def userToRegResp(ua: UserAccount): Option[JsValue] = Option(regResp write RegistrationResponse(ua
    .user_id))

  protected implicit def optUserIdToRegResp(uid: Option[String]): Option[JsValue] = uid map { uid =>
    regResp write RegistrationResponse(uid)
  }

}


/** The base format of every response. All communications must have this format at the root level. */
case class FlowResponse(
                         var response: Option[JsValue] = None,
                         var error_code: Option[String] = None,
                         var error_msg: Option[String] = None
                       ) {
}

object FlowResponse {
  def success(response: JsValue): FlowResponse = FlowResponse(Option(response))

  def failure(e: FlowError): FlowResponse = FlowResponse(None, Option(e.errorCode), Option(e.message))
}

