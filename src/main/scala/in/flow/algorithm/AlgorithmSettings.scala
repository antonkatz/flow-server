package in.flow.algorithm

object AlgorithmSettings {
  /** How deep to search for connections, eg friends only, friends of friends, friends of friends of friends */
  val connections_search_depth = 3 //friends of friends of friends

  /** The smallest possible amount for a transaction, in hours (6 min) */
  val transaction_amount_lower_bound: BigDecimal = 0.1

  /** in what time the financial principal doubles, in seconds */
  val principle_double_in: Double = 60 * 60 * 24 * 7
}
