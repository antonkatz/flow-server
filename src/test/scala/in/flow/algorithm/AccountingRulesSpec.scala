package in.flow.algorithm

import in.flow.commformats.ExternalCommFormats.OfferRequest
import in.flow.commformats.InternalCommFormats._
import in.flow.db.{Db, DbSchema}
import in.flow.getLogger
import in.flow.users.{Connections, Offers, UserAccount, Wallet}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import scribe.Logger
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


/**
  * For testing encryption filters on the servlet
  */
class AccountingRulesSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  private val logger: Logger = "AccountingSpec"
  var for_cleanup = Set[Transaction]()

  "Accounting" when {
    val u1: UserAccount = UserAccount("acc1", "what a friend", in.flow_test.mock_public_key)
    val u2: UserAccount = UserAccount("acc2", "you call", in.flow_test.mock_public_key)
    val ins = Db.run(DBIO.seq(DbSchema.user_accounts += u1.storable, DbSchema.user_accounts += u2.storable))
    Await.ready(ins, Duration.Inf)

    Await.ready(Connections.connectUsers(u1, u2, UserConnectionType.friend), Duration.Inf)


    "working with interest" should {
      "calculate positive for a wallet with negative balance AND a negative for a wallet with positive balance" in {
        val amount: BigDecimal = 10

        // u2 owes u1
        val oreq = OfferRequest(u2.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u1) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        Thread.sleep(1000)

        val w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        val w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)

        val w2_ui: BigDecimal = w2.right.get.uncommitted_interest.get
        assert(w1.right.get.uncommitted_interest.get < 0)
        assert(w2_ui > 0)
        assert(w2.right.get.balance.get == -amount)
        assert(w1.right.get.balance.get == amount)
        assert(trs.right.get.size == 1)
        for_cleanup ++= trs.right.get
      }

      "have interest applied at proper interval" in {
        var amount: BigDecimal = 1
        var oreq = OfferRequest(u1.user_id, amount, None)
        // u1 owes u2
        var trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        var trs = Await.result(trsf, Duration.Inf)

        var w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        var w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        // todo. incorrect
        var b2 = w2.right.get.balance.get + w2.right.get.interest.get
        var b1 = w1.right.get.balance.get + w1.right.get.interest.get
        assert(w2.right.get.interest.getOrElse(0: BigDecimal) > 0)
        assert(w1.right.get.interest.getOrElse(0: BigDecimal) < 0)
        assert(w2.right.get.transactions.count(_.isInstanceOf[InterestTransaction]) == 1)
        assert(w1.right.get.transactions.count(_.isInstanceOf[InterestTransaction]) == 1)
        assert(b2 > -9 & b2 < -8.9)
        assert(b1 < 9 & b1 > 8.9)
        assert(trs.right.get.size == 1)

        // this closely run, there should not be another interest calculation

        oreq = OfferRequest(u1.user_id, amount, None)
        trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        trs = Await.result(trsf, Duration.Inf)

        w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        var i1 = w1.right.get.interest.get
        var i2 = w2.right.get.interest.get
//        b2 = w2.right.get.principal.get + i2
//        b1 = w1.right.get.principal.get + i1
        b2 = w2.right.get.balance.get
        b1 = w1.right.get.balance.get
        assert(w2.right.get.interest.getOrElse(0: BigDecimal) > 0)
        assert(w1.right.get.interest.getOrElse(0: BigDecimal) < 0)
        assert(w2.right.get.transactions.count(_.isInstanceOf[InterestTransaction]) == 1)
//        assert(w2.right.get.principal.get == -8)
        assert(b2 > -8 & b2 < -7.9)
        assert(b1 < 8 & b1 > 7.9)

        // now there should be another interest calculation
        Thread.sleep(2001)

        oreq = OfferRequest(u1.user_id, amount, None)
        trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        trs = Await.result(trsf, Duration.Inf)

        w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        assert(w2.right.get.interest.getOrElse(0: BigDecimal) > 0)
        assert(w2.right.get.transactions.count(_.isInstanceOf[InterestTransaction]) == 2)
        assert(w2.right.get.balance.get > 6.9 && w2.right.get.balance.get < 7)
        assert(w1.right.get.interest.get < i1)
        assert(w2.right.get.interest.get > i2)

      }
    }
  }

  override def afterAll(): Unit = {
      val delete_ids = for_cleanup.map(_.transaction_id)
//      val q = DbSchema.transactions.filter(t => t.transactionId inSet delete_ids)
      val q = DbSchema.transactions
      val affected = Await.result(Db.run(q.delete), Duration.Inf)
      affected should be > (0)
  }
}

