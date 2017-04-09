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
    val u3: UserAccount = UserAccount("acc3", "third wheel", in.flow_test.mock_public_key)
    val ins = Db.run(DBIO.seq(DbSchema.user_accounts += u3.storable, DbSchema.user_accounts += u1.storable, DbSchema
      .user_accounts += u2.storable))
    Await.ready(ins, Duration.Inf)

    Await.ready(Connections.connectUsers(u1, u2, UserConnectionType.friend), Duration.Inf)
    Await.ready(Connections.connectUsers(u1, u3, UserConnectionType.friend), Duration.Inf)


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
        var b2 = w2.right.get.balance.get
        var b1 = w1.right.get.balance.get
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
        assert(w2.right.get.balance.get < -6.9 && w2.right.get.balance.get > -7)
        assert(w1.right.get.interest.get < i1)
        assert(w2.right.get.interest.get > i2)

      }

      "properly account with 3 people" in {
        var amount: BigDecimal = 1
        var oreq = OfferRequest(u1.user_id, amount, None)
        var trsf = Offers.createOffer(oreq, u3) flowWith Wallet.performTransaction
        var trs = Await.result(trsf, Duration.Inf)

        var w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w3 = Await.result(Wallet.getWallet(u3) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get

        w1.balance.get.doubleValue() should be (6.0 +- 0.1)
        w2.balance.get.doubleValue() should be (-7.0 +- 0.1)
        w3.balance.get should be (amount)
      }

      "u3 giving to u2" in {
        var amount: BigDecimal = 2
        var oreq = OfferRequest(u3.user_id, amount, None)
        var trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        var trs = Await.result(trsf, Duration.Inf)

        var w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w3 = Await.result(Wallet.getWallet(u3) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get

        w1.balance.get.doubleValue() should be (6.0 +- 0.1)
        w2.balance.get.doubleValue() should be (-5.0 +- 0.1)
        w3.balance.get.doubleValue() should be (-1.0 +- 0.1)
      }

      "u1 giving to u3" in {
        var amount: BigDecimal = 7
        var oreq = OfferRequest(u1.user_id, amount, None)
        var trsf = Offers.createOffer(oreq, u3) flowWith Wallet.performTransaction
        var trs = Await.result(trsf, Duration.Inf)

        var w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w3 = Await.result(Wallet.getWallet(u3) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get

        w1.balance.get.doubleValue() should be (-1.0 +- 0.1)
        w2.balance.get.doubleValue() should be (-5.0 +- 0.1)
        w3.balance.get.doubleValue() should be (6.0 +- 0.1)
      }

      "u3 giving to u2 second time" in {
        var amount: BigDecimal = 6
        var oreq = OfferRequest(u3.user_id, amount, None)
        var trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        var trs = Await.result(trsf, Duration.Inf)

        var w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w3 = Await.result(Wallet.getWallet(u3) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get

        w1.balance.get.doubleValue() should be (-1.0 +- 0.1)
        w2.balance.get.doubleValue() should be (1.0 +- 0.1)
        w3.balance.get.doubleValue() should be (0.0 +- 0.1)
      }

      "u2 use interest from u3" in {
        var amount: BigDecimal = 1
        var oreq = OfferRequest(u2.user_id, amount, None)
        var trsf = Offers.createOffer(oreq, u1) flowWith Wallet.performTransaction
        var trs = Await.result(trsf, Duration.Inf)

        var w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        var w3 = Await.result(Wallet.getWallet(u3) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get

        w1.balance.get.doubleValue() should be (0.0 +- 0.1)
        w2.balance.get.doubleValue() should be (0.0 +- 0.1)
        w3.balance.get.doubleValue() should be (0.0 +- 0.1)
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

