package in.flow.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import in.flow.FlowResponseType
import in.flow.users.UserAccount
import in.flow.users.registration.{RegistrationRequest, RegistrationResponse}
import spray.json.{RootJsonFormat, _}

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val regReq = jsonFormat2(RegistrationRequest)
  implicit val regResp = jsonFormat1(RegistrationResponse)
  implicit val flowResp = jsonFormat3(FlowResponse)

  implicit def toFlowResponse[T](res: FlowResponseType[T])(implicit t: (T) => Option[JsValue]): FlowResponse = {
    res fold(
      fail => FlowResponse(None, Option(fail.errorCode), Option(fail.message)),
      s => FlowResponse(t(s))
    )
  }
}

case class FlowResponse (
  var response: Option[JsValue] = None,
  var error_code: Option[String] = None,
  var error_msg: Option[String] = None
)