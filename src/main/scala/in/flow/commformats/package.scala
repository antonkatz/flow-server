package in.flow

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsValue}
import collection.Iterable

/**
  * Created by anton on 15/03/17.
  */
package object commformats extends SprayJsonSupport with DefaultJsonProtocol {

  case class RegistrationRequest(invitation_code: String, display_name: String)

  case class RegistrationResponse(id: String)

  /** @param fof friends of friends */
  case class ConnectionsResponse(friends: Iterable[UserRepresentation], fof: Seq[Iterable[UserRepresentation]])

  case class UserRepresentation(user_id: String, display_name: String)

  def failedFlowResponse(e: FlowError): FlowResponse = FlowResponse(None, Option(e.errorCode), Option(e.message))

  /** The base format of every response. All communications must have this format at the root level. */
  case class FlowResponse(
                           var response: Option[JsValue] = None,
                           var error_code: Option[String] = None,
                           var error_msg: Option[String] = None
                         ) {
  }

  object FlowResponse {
    def success(response: JsValue): FlowResponse = FlowResponse(Option(response))
  }

}
