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
    val builder = (parent_id: Option[TransactionPointer], amount: BigDecimal) => {
      val now = getNow
      val t_id = generateId(offer.from, offer.to, offer.hours)

      OfferTransaction(t_id, parent_id, from = offer.from, to = offer.to,
        amount = amount, now, offer = offer)
    }

    getWallet(offer.from) flowWith processInterest flowWith {wallet =>
      Offers.completeOffer(offer) flowWith { _ =>
        evolveTransactions(wallet, offer.hours, builder) }
    }
  }

  /** finds out how much interest is not committed, creates a transaction for that amount and stores it */
  private def processInterest(wallet: UserWallet): FutureErrorFlow[UserWallet] = {
    AccountingRules.getInterestToCommit(wallet).fold({
      // if no interest to commit
      Future(Right(wallet)) : FutureErrorFlow[UserWallet]
    })({amount =>
      val t_id = generateId(wallet.owner, wallet.owner, amount)
      val t = InterestTransaction(t_id, None, from = wallet.owner, to = wallet.owner, amount, getNow)
      store(t) flowRight {t => updateWallet(wallet, t)
        // modifying the wallet to reflect this change
      }
    })
  }

  /** All updates MUST use this method
    * does all the proper routines when adding a transaction to an existing wallet */
  def updateWallet(w: UserWallet, t: Transaction): UserWallet = t match {
    case t: InterestTransaction =>
      // todo. instead use loadauxwalletinfo
      val upd_int = w.interest.map(_ + t.amount)
      val upd_trs = w.transactions :+ t
      val upd_open_trs = findOpenTransactions(w.owner, upd_trs)
      w.copy(uncommitted_interest = None, interest = upd_int, transactions = upd_trs,
                          open_transactions = upd_open_trs)
  }

  private def generateId(from: UserAccountPointer, to: UserAccountPointer, amount: BigDecimal) = {
    val now = getNow
    val gen_id_from = from.user_id + to.user_id + amount.toString() + now + Random.nextDouble()
    val t_id_bytes = global_sha.digest(gen_id_from.getBytes(global_string_format))
    Base64.getEncoder.encodeToString(t_id_bytes)
  }

  /** takes inflowing transactions of the `from` user, and closes them up as a transfer to the `to` user
    * must not be used for interest
    * checks that the amount is more than 0
    * @param transBuilder a functions taking `parent_id` and `amount`, returning a [[Transaction]] */
  private[users] def evolveTransactions(wallet: UserWallet, amount: BigDecimal,
                                        transBuilder: (Option[TransactionPointer], BigDecimal) => Transaction):
  FutureErrorFlow[Iterable[Transaction]] = {
    if (amount < 0) {
      logger.error("Transaction attempt of negative amount")
      return Future(Left(InvalidInputError("you cannot transfer negative amounts")))
    }

    val from = wallet.owner
    // childless, sorted, oldest first
    var open_transactions = getOpenTransactions(wallet).sortBy(_.timestamp)
    var remaining_balance = amount
    var new_transactions = Seq[Transaction]()

    // splitting existing transactions into new transactions
    while (remaining_balance > 0) {
      open_transactions.headOption match {
        case Some(open_trans) =>
          val amount_of_new = if (open_trans.amount < remaining_balance) open_trans.amount else remaining_balance
          remaining_balance -= open_trans.amount

          // in all cases where there is a remaining balance and open transactions
          new_transactions :+= transBuilder(Option(open_trans), amount_of_new)
          // old transaction is larger than the remaining balance
          if (amount_of_new < open_trans.amount) {
            // this code should be executed at most once
            val backflow_id = generateId(from, from, -remaining_balance)
            new_transactions :+= BackflowTransaction(backflow_id, Option(open_trans), from, from,
              -1 * remaining_balance, getNow)
          }

          open_transactions = open_transactions.tail
        case _ =>
          // case ran out of open transactions
          new_transactions :+= transBuilder(None, remaining_balance)
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
    transactions filter { t => !(parent_ids contains t.transaction_id) & (t.to == owner) }
  }

  /** @see [[AccountingRules.loadPrincipal()]] [[AccountingRules.loadInterest()]] */
  def loadAuxWalletInfo(wallet: UserWallet): UserWallet = {
    var w = AccountingRules.loadPrincipal(wallet)
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
      case tt if tt == TransactionType.interest =>
        Option(InterestTransaction(t.transaction_id, parent, from = from, to = to, t.amount, t.timestamp.toInstant))
    }
  }

}
