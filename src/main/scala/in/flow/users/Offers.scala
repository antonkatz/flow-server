package in.flow.users

import in.flow.WithErrorFlow
import in.flow.commformats.{OfferRequest, OfferResponse}

import scala.concurrent.Future

/**
  * Creating offers, accepting and rejecting
  * Checks for validity of offers made
  */
object Offers {

  def createOffer(request: OfferRequest): Future[WithErrorFlow[OfferResponse]] = {
    ???
  }

  /** NOT SAFE
    * @return offer id */
  private def storeOffer(r: OfferRequest): Future[String] = {

  }

  /** creates an id for an offer */
  private def produceId(id: Int) = {

  }
}
