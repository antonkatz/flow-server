package in.flow.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import in.flow.users.registration.{RegistrationRequest, RegistrationResponse}
import spray.json._

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val regReq = jsonFormat2(RegistrationRequest)
  implicit val regResp = jsonFormat3(RegistrationResponse)
}