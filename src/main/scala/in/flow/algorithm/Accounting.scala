package in.flow.algorithm

import in.flow.commformats.InternalCommFormats.{Transaction, UserWallet}
import in.flow.users.UserAccountPointer

/**
  * Created by anton on 23/03/17.
  */
object Accounting {

  /** @return the amount with a sign indicating if the transaction was inflowing or an outflowing*/
  def getRelativeAmount(u: UserAccountPointer, t: Transaction): BigDecimal =
    if (u == t.from) t.amount * -1 else t.amount

  /** @return sum of all user transactions */
  def loadCommittedBalance(wallet: UserWallet): UserWallet = {
    val raf = (t: Transaction) => Accounting.getRelativeAmount(wallet.owner, t)
    val balance = wallet.transactions.map(raf).sum
    wallet.copy(committed_balance = Option(balance))
  }
}
