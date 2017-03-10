package in.flow.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import in.flow.users.UserAccount
import in.flow.users.registration.Registrar.Response
import in.flow.users.registration.{RegistrationRequest, RegistrationResponse}
import spray.json.{RootJsonFormat, _}

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val regReq = jsonFormat2(RegistrationRequest)
  implicit val regResp = jsonFormat1(RegistrationResponse)
  implicit val flowResp = jsonFormat3(FlowResponse)

  implicit def registrationResultToResponse(res: Response[UserAccount]): FlowResponse = {
    res fold(
      fail => FlowResponse(None, Option(fail.errorCode), Option(fail.message)),
      ua => {
        val rr: JsValue = regResp.write(RegistrationResponse(Some(ua.user_id)))
        FlowResponse(Some(rr))
      }
    )
  }
}

case class FlowResponse (
  var response: Option[JsValue] = None,
  var error_code: Option[String] = None,
  var error_msg: Option[String] = None
)