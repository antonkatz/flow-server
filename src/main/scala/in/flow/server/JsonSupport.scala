package in.flow.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import in.flow.{FlowResponseType, UnknownError}
import in.flow.commformats._
import in.flow.users.UserAccount
import spray.json.{RootJsonFormat, _}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import JsonWriter.func2Writer

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val regReq = jsonFormat2(RegistrationRequest)
  implicit val regResp = jsonFormat1(RegistrationResponse)
  implicit val flowResp = jsonFormat3(FlowResponse.apply)
  implicit def userRep = jsonFormat2(UserRepresentation)
//  implicit def connLevel = iterableFormat[UserRepresentation]
  implicit def connResp = jsonFormat(ConnectionsResponse, "friends", "fof")

  implicit def toFlowResponse[T](res: FlowResponseType[T])(implicit t: (T) => Option[JsValue]): FlowResponse = {
    res fold(
      fail => FlowResponse(None, Option(fail.errorCode), Option(fail.message)),
      s => FlowResponse(t(s))
    )
  }
  def toFlowResponse[T](what: Option[T])(implicit t: (T) => JsValue): FlowResponse = {
    FlowResponse(what map {w => t(w)})
  }

  protected def connectionsToFlowResponse(cons_f: Future[Seq[Set[UserAccount]]]): Future[FlowResponse] = {
    cons_f map {cons =>
      val fof = cons.tail map connectionsToRepresentations
      val friends = connectionsToRepresentations(cons.head)
      val r = ConnectionsResponse(friends, fof)
      val json : JsValue = r.toJson
      FlowResponse.success(json)
    } recover {
      case _:NoSuchElementException =>
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
    regResp write RegistrationResponse(uid)}

}

