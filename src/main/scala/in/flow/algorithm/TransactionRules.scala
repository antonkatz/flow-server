package in.flow.algorithm

import in.flow.FutureErrorFlow
import in.flow.commformats.InternalCommFormats.{Offer, Transaction, UserConnectionType}
import in.flow.users.{Connections, Wallet}

/**
  * A higher level over basic user directives
  *
  * - Performs transactions by chaining [[in.flow.users.Wallet]] and [[in.flow.users.Connections]] methods
  */
object TransactionRules {

  /** performs the transaction using the [[in.flow.users.Wallet]] method, and in case of success connects the two
    * users as friends if that was not already the case */
  def performTransaction(offer: Offer): FutureErrorFlow[Iterable[Transaction]] = {
    Wallet.performTransaction(offer) flowComplete(_ =>
      Connections.connectUsers(from = offer.from, to = offer.to,UserConnectionType.friend)
      )
  }
}
