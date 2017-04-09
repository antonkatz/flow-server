package in.flow.algorithm

import in.flow.commformats.InternalCommFormats.{BackflowTransaction, InterestTransaction, Transaction, UserWallet}
import in.flow.users.{UserAccountPointer, Wallet}
import in.flow.getNow
import in.flow.users.Wallet.generateId
/**
  * Created by anton on 23/03/17.
  */
object AccountingRules {

  /** @return the amount with a sign indicating if the transaction was inflowing or an outflowing*/
  def getRelativeAmount(u: UserAccountPointer, t: Transaction): BigDecimal =
    if (u != t.to) t.amount * -1 else t.amount

  /** sums up all open non-interest transactions in a wallet, (that have a parent and are not to the owner)
    * @return sum of all user transactions that are not interest transactions; if the balance is negative, returns 0,
    *         because it does not account for transactions with no parent */
  def loadBalance(wallet: UserWallet): UserWallet = {
    val open = Wallet.getOpenTransactions(wallet)
    val p = getSumOfType(open, wallet.owner, (t) => !t.isInstanceOf[InterestTransaction])
//    val p = getSumOfType(wallet.transactions, wallet.owner,
//      (t) => !(t.isInstanceOf[InterestTransaction] | t.isInstanceOf[BackflowTransaction]))
    wallet.copy(balance = Option(p))
  }

  def loadInterest(wallet: UserWallet): UserWallet = {
    val open = Wallet.getOpenTransactions(wallet)
    val i = getSumOfType(open, wallet.owner, (t) => t.isInstanceOf[InterestTransaction])
    wallet.copy(interest = Option(i))
  }

  /**@return interest that can be committed at this point, or None if the interval between applications haven't
    *         passed yet */
  def getInterestToCommit(wallet: UserWallet): Option[Iterable[InterestTransaction]] = {
    val last_interest = wallet.transactions.collect {
      case t: InterestTransaction => t
    }.sortBy(_.timestamp).reverse.headOption

    if (last_interest.isEmpty ||
      (getNow.toEpochMilli - last_interest.get.timestamp.toEpochMilli) > AlgorithmSettings.commit_interest_every) {
      Option(generateInterest(wallet))
    } else None
  }

  def generateInterest(wallet: UserWallet): Iterable[InterestTransaction] = {
    val open_trs = Wallet.getOpenTransactions(wallet) filterNot(_.isInstanceOf[InterestTransaction])
    val now = getNow
    open_trs map { t =>
      val time_diff: BigDecimal = (BigDecimal(now.toEpochMilli) - t.timestamp.toEpochMilli) / 1000
      val rate = getPerTimeInterestRate(time_diff) - 1

      val a = t.amount * rate
      val t_id = Wallet.generateId(t.from, t.to, a)
      InterestTransaction(t_id, parent = Option(t), from = t.to, to = t.from, a, getNow)
    }
  }

  /** take inflowing transactoins, disregarding outflowing transactions, and calculate interest upon those */
  def loadUncommitedInterest(wallet: UserWallet): UserWallet = {
    val interest = generateInterest(wallet) map { t =>
      getRelativeAmount(wallet.owner, t)
    } sum;

    wallet.copy(uncommitted_interest = Option(interest))
  }

  private def getSumOfType(wallet: UserWallet, typeCheck: (Transaction) => Boolean): BigDecimal = {
    getSumOfType(wallet.transactions, wallet.owner, typeCheck)
  }

  private def getSumOfType(transactions: Seq[Transaction], owner: UserAccountPointer,
                           typeCheck: (Transaction) => Boolean): BigDecimal = {
    val raf = (t: Transaction) => AccountingRules.getRelativeAmount(owner, t)
    transactions.filter(typeCheck).map(raf).sum
  }

  /** @param time_unit in seconds*/
  def getPerTimeInterestRate(time_unit: Long): BigDecimal = getPerTimeInterestRate(BigDecimal(time_unit))

  def getPerTimeInterestRate(time_unit: BigDecimal): BigDecimal = {
    val num_of_compounds = AlgorithmSettings.principle_halflife / time_unit
    Math.pow(2, (1 / num_of_compounds).toDouble)
  }
}
