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
  case class OfferRequest(to_user_id: String, hours: Float, description: Option[String])

  case class OfferResponse(offer_id: String, from_user_id: String, hours: Float, description: Option[String] = None)
  {
    def withDescription(description: String): OfferResponse = {
      val d = if (description.isEmpty) None else Option(description)
      this.copy(description = d)
    }
  }

  case class OffersResponse(offers: Iterable[OfferResponse])
}
