package in.flow.users

import java.sql.Timestamp
import java.util.Base64

import in.flow.algorithm.AccountingRules
import in.flow.commformats.InternalCommFormats.{Transaction, TransactionType, UserWallet, _}
import in.flow.db.{Db, DbSchema, TransactionStorable}
import in.flow.{DatabaseError, FutureErrorFlow, _}
import scribe.Logger
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

/**
  * Responsible for all transactions interactions, balances, and financial interest operations
  *
  */
object Wallet {
  private val logger: Logger = "Wallet"

  type TransactionResult = FutureErrorFlow[Transaction]

  /** performs a transaction, checking that the user is allowed to do so
    * WARNING, currently does not check didly squat */
  def performTransaction(from: UserAccount, to: UserAccount, amount: BigDecimal): TransactionResult = {
    throw new NotImplementedError("creating transaction only from offers at this moment")
  }

  /** performs a transaction, checking that the user is allowed to do so
    * also applies interest before offer completion
    * WARNING, currently does not check didly squat */
  def performTransaction(offer: Offer): FutureErrorFlow[Iterable[Transaction]] = {
//    val builder = (parent_id: Option[TransactionPointer], from: UserAccountPointer, amount: BigDecimal) => {
//      val now = getNow
//      val t_id = generateId(offer.from, offer.to, offer.hours)
//
//      OfferTransaction(t_id, parent_id, from = from, to = offer.from,
//        amount = amount, now, offer = offer)
//    }

    getWallet(offer.to) flowWith processInterest flowWith {wallet =>
      Offers.completeOffer(offer) flowWith { _ =>
        evolveTransactions(wallet, offer.hours, offer.from) }
    }
  }

  /** finds out how much interest is not committed, creates a transaction for that amount and stores it */
  private def processInterest(wallet: UserWallet): FutureErrorFlow[UserWallet] = {
    AccountingRules.getInterestToCommit(wallet).fold({
      // if no interest to commit
      Future(Right(wallet)) : FutureErrorFlow[UserWallet]
    })({interest_trs =>
      // when interest flows out, the parent transaction needs to be closed up and split, so as to keep providing
      // the interest to the person who sent the parent transaction
      val backflows = interest_trs map {it =>
        val pt = wallet.transactions.find(_.transaction_id == it.parent.get.transaction_id).get
        val backflow_id = generateId(pt.from, pt.to, pt.amount)
        val a = pt.amount - it.amount
        BackflowTransaction(backflow_id, parent = Option(pt), from = pt.from, to = pt.to, a, getNow)
      }
      store(interest_trs ++ backflows) flowRight {trs => updateWallet(wallet, trs)
        // modifying the wallet to reflect this change
      }

    })
  }

  /** All updates MUST use this method
    * does all the proper routines when adding a transaction to an existing wallet */
  def updateWallet(w: UserWallet, trs: Iterable[Transaction]): UserWallet = {
      // todo. instead use loadauxwalletinfo
      val upd_trs = w.transactions ++ trs
      val upd_open_trs = findOpenTransactions(w.owner, upd_trs)
      val upd_w = w.copy(balance = None, uncommitted_interest = None, interest = None, transactions = upd_trs,
                          open_transactions = upd_open_trs)
    AccountingRules.loadInterest(upd_w)
  }

  def generateId(from: UserAccountPointer, to: UserAccountPointer, amount: BigDecimal): String = {
    val now = getNow
    val gen_id_from = from.user_id + to.user_id + amount.toString() + now + Random.nextDouble()
    val t_id_bytes = global_sha.digest(gen_id_from.getBytes(global_string_format))
    Base64.getEncoder.encodeToString(t_id_bytes)
  }

  /** takes inflowing transactions of the `from` user, and closes them up as a transfer to the `to` user
    * must not be used for interest
    * checks that the amount is more than 0 */
  private[users] def evolveTransactions(wallet: UserWallet, amount: BigDecimal, to: UserAccountPointer):
  FutureErrorFlow[Iterable[Transaction]] = {
    if (amount < 0) {
      logger.error("Transaction attempt of negative amount")
      return Future(Left(InvalidInputError("you cannot transfer negative amounts")))
    }

    val from = wallet.owner
    // childless, sorted, oldest first
    var evolvable_dbg = getOpenTransactions(wallet).filter(_.to == wallet.owner).sortBy(_.timestamp)
    var evolvable = getOpenTransactions(wallet).filter(_.to == wallet.owner).sortBy(_.timestamp)
    var remaining_balance = amount
    var new_transactions = Seq[Transaction]()

    // backflowing the existsing transaction if there is enough balance
    // or creating new money
    while (remaining_balance > 0) {
      evolvable.headOption match {
        case Some(open_trans) =>
          val shrunk_coin_size = if (open_trans.amount < remaining_balance) {
            BigDecimal(0)
          } else {
            open_trans.amount - remaining_balance
          }
          remaining_balance -= open_trans.amount

          val backflow_id = generateId(open_trans.from, open_trans.to, shrunk_coin_size)
          new_transactions :+= BackflowTransaction(backflow_id, Option(open_trans), from = open_trans.from,
            to = open_trans.to, shrunk_coin_size, getNow)

          if (to != open_trans.from) {
            val transferred_coin_size = open_trans.amount - shrunk_coin_size
            val reg_id = generateId(open_trans.from, to, transferred_coin_size)
            new_transactions :+= RegularTransaction(reg_id, Option(open_trans), from = open_trans.from,
            to = to, transferred_coin_size, getNow)
          }

          evolvable = evolvable.tail
        case _ =>
          val id = generateId(from, to, -remaining_balance)
          // case ran out of open transactions
          new_transactions :+= RegularTransaction(id, None, from=from, to=to, remaining_balance, getNow)
          remaining_balance = 0
      }
    }

    store(new_transactions)
  }

  /** @return incoming transactions from wallet that do not have any children */
  def getOpenTransactions(wallet: UserWallet): Seq[Transaction] = wallet.open_transactions

  /** @return incoming transactions from wallet that do not have any children */
  private def findOpenTransactions(owner: UserAccountPointer, transactions: Seq[Transaction]): Seq[Transaction] = {
    val parent_ids = transactions collect {
      case p if p.parent isDefined => p.parent.get.transaction_id
    }
    transactions filter { t => !(parent_ids contains t.transaction_id) & t.amount != 0}
//    transactions filter { t => !(parent_ids contains t.transaction_id) | (t.parent.isEmpty & t.from == owner)}
  }

  /** @see [[AccountingRules.loadBalance()]] [[AccountingRules.loadInterest()]] */
  def loadAuxWalletInfo(wallet: UserWallet): UserWallet = {
    var w = AccountingRules.loadBalance(wallet)
    w = AccountingRules.loadInterest(w)
    AccountingRules.loadUncommitedInterest(w)
  }

  /** HOT. The database access can fail giving wrong balances
    *
    * @return all transaction amounts summed up */
  def getWallet(user: UserAccountPointer): FutureErrorFlow[UserWallet] = {
    val q = DbSchema.transactions.filter(t => t.to === user.user_id || t.from === user.user_id).result

    Db.run(q) map { dbtrs =>
      val trs = dbtrs map fromStorable
      if (trs.exists(_.isEmpty)) {
        Left(DatabaseError("we messed up, sorry"))
      } else {
        val trsf = trs.flatten
        Right(UserWallet(user, trsf, open_transactions = findOpenTransactions(user, trsf)))
      }
    } recover {
      case e => logger.error(s"Failed during database access when getting wallet balance for user ${user.user_id}: " +
        s"${e.getMessage}")
        Left(DatabaseError("we couldn't get your balance"))
    }
  }

  private def store[T <: Transaction](t: T): FutureErrorFlow[T] = {
    val storable = asStorable(t)
    Db.run(DbSchema.transactions += storable) map { _ => Right(t) } recover {
      case e => logger.error(s"Could not store transaction: ${e.getMessage}")
        Left(DatabaseError("we couldn't store this transaction"))
    }
  }

  private def store[T <: Transaction](t: Iterable[T]): FutureErrorFlow[Iterable[T]] = {
    val storable = t map asStorable
    Db.run(DbSchema.transactions ++= storable) map { _ => Right(t) } recover {
      case e => logger.error(s"Could not store transaction: ${e.getMessage}")
        Left(DatabaseError("we couldn't store this transaction"))
    }
  }


  /** @return instance that can be put into a database */
  private def asStorable(t: Transaction): TransactionStorable = {
    val time = Timestamp.from(t.timestamp)
    val pid = t.parent.map(_.transaction_id)
    val st = TransactionStorable(t.transaction_id, parent_id = pid, from_user_id = t.from.user_id,
      to_user_id = t.to.user_id, amount = t.amount, timestamp = time,
      offer_id = None, transaction_type = t.transaction_type.toString)
    t match {
      case t: OfferTransaction => st.copy(offer_id = Option(t.offer.offer_id))
      case _ => st
    }
  }

  import in.flow.commformats.InternalCommFormats.TransactionPointer

  /** HOT. does not match all types of transactions */
  private def fromStorable(t: TransactionStorable): Option[Transaction] = {
    val parent = t.parent_id map TransactionPointer.apply
    val from = UserAccountPointer(t.from_user_id)
    val to = UserAccountPointer(t.to_user_id)
    TransactionType.withName(t.transaction_type) match {
      case tt if tt == TransactionType.offer => t.offer_id map { oid =>
        val offer = new OfferPointer {
          override val offer_id: String = oid
        }
        OfferTransaction(t.transaction_id, parent, from = from, to = to, t.amount, t.timestamp.toInstant, offer)
      }
      case tt if tt == TransactionType.backflow =>
        Option(BackflowTransaction(t.transaction_id, parent, from = from, to = to, t.amount, t.timestamp.toInstant))
      case tt if tt == TransactionType.regular =>
        Option(BackflowTransaction(t.transaction_id, parent, from = from, to = to, t.amount, t.timestamp.toInstant))
      case tt if tt == TransactionType.interest =>
        Option(InterestTransaction(t.transaction_id, parent, from = from, to = to, t.amount, t.timestamp.toInstant))
    }
  }

}
