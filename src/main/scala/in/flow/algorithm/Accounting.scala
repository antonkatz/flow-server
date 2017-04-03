package in.flow.algorithm

import in.flow.commformats.InternalCommFormats.{InterestTransaction, Transaction, UserWallet}
import in.flow.users.{UserAccountPointer, Wallet}
import in.flow.getNow
/**
  * Created by anton on 23/03/17.
  */
object Accounting {

  /** @return the amount with a sign indicating if the transaction was inflowing or an outflowing*/
  def getRelativeAmount(u: UserAccountPointer, t: Transaction): BigDecimal =
    if (u == t.from) t.amount * -1 else t.amount

  /** @return sum of all user transactions that are not interest transactions */
  def loadPrincipal(wallet: UserWallet): UserWallet = {
    wallet.copy(principal = Option(getSumOfType(wallet, (t) => !t.isInstanceOf[InterestTransaction])))
  }

  def loadInterest(wallet: UserWallet): UserWallet = {
    wallet.copy(interest = Option(getSumOfType(wallet, (t) => t.isInstanceOf[InterestTransaction])))
  }

  /** take inflowing transactoins, disregarding outflowing transactions, and calculate interest upon those */
  def loadUncommitedInterest(wallet: UserWallet): UserWallet = {
    val open_trs = Wallet.findOpenTransactions(wallet)
    val now = getNow
    var interest = open_trs map {t =>
      val tdiff: BigDecimal = (BigDecimal(now.toEpochMilli) - t.timestamp.toEpochMilli) / 1000
      val rate = getPerTimeInterestRate(tdiff) - 1
      t.amount * rate
    } sum;

    wallet.copy(uncommitted_interest = Option(interest))
  }

  private def getSumOfType(wallet: UserWallet, typeCheck: (Transaction) => Boolean): BigDecimal = {
    val raf = (t: Transaction) => Accounting.getRelativeAmount(wallet.owner, t)
    wallet.transactions.filter(typeCheck).map(raf).sum
  }

  /** @param time_unit in seconds*/
  def getPerTimeInterestRate(time_unit: Long): BigDecimal = getPerTimeInterestRate(BigDecimal(time_unit))

  def getPerTimeInterestRate(time_unit: BigDecimal): BigDecimal = {
    val num_of_compounds = AlgorithmSettings.principle_double_in / time_unit
    Math.pow(2, (1 / num_of_compounds).toDouble)
  }

//  def getUncommittedInterest(wallet: UserWallet): BigDecimal = {
//    val inflow = wallet.transactions.filter(_.to == wallet.owner)
//    val now = getNow
//    inflow map {t =>
//      val passed_time = t.timestamp minusNanos now.getNano getEpochSecond;
//      t.amount * getPerTimeInterestRate(passed_time)
//    } sum
//  }
}
