package in.flow

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsValue}
import collection.Iterable

/**
  * Created by anton on 15/03/17.
  */
package object commformats {

  case class RegistrationRequest(invitation_code: String, display_name: String)

  case class RegistrationResponse(id: String)

  /** @param fof friends of friends */
  case class ConnectionsResponse(friends: Iterable[UserRepresentation], fof: Seq[Iterable[UserRepresentation]])

  case class UserRepresentation(user_id: String, display_name: String)

  /* offers */

  /** for creating an offer */
  case class OfferRequest(to_user_id: String, hours: Float, description: String)

  case class OfferResponse(offer_id: String)
}
