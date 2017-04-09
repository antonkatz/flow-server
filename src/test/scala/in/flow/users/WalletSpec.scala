package in.flow.users

import in.flow.commformats.ExternalCommFormats.OfferRequest
import in.flow.commformats.InternalCommFormats._
import in.flow.db.{Db, DbSchema}
import in.flow.getLogger
import org.scalatest.{Matchers, WordSpec}
import scribe.Logger
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random
import in.flow._


/**
  * For testing encryption filters on the servlet
  */
class WalletSpec extends WordSpec with Matchers {
  private val logger: Logger = "WalletSpec"
  var for_cleanup = Set[Transaction]()

  "Wallet" when {
    val u1: UserAccount = UserAccount("wallet1", "what a friend", in.flow_test.mock_public_key)
    val u2: UserAccount = UserAccount("wallet2", "you call", in.flow_test.mock_public_key)
    val ins = Db.run(DBIO.seq(DbSchema.user_accounts += u1.storable, DbSchema.user_accounts += u2.storable))
    Await.ready(ins, Duration.Inf)

    Connections.connectUsers(u1, u2, UserConnectionType.friend)


    "performing transactions" should {
      var firstPrimordial: Transaction = null
      "create a primordial one if there are no open ones" in {
        val amount: BigDecimal = 10
        val oreq = OfferRequest.apply(u2.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u1) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        trs.isInstanceOf[Right[_,_]] should be(true)
        trs.right.get should have size(1)
        trs.right.get.exists(t => t.amount == amount && t.parent.isEmpty) should be(true)
        trs.right.get.filterNot(_.isInstanceOf[BackflowTransaction]).map(_.amount).sum should be(amount)
        firstPrimordial = trs.right.get.head

        for_cleanup ++= trs.right.get
      }

      "create another primordial one" in {
        val amount: BigDecimal = 10
        val oreq = OfferRequest.apply(u2.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u1) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        val w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get

        trs.isInstanceOf[Right[_,_]] should be(true)
//        trs.right.get should have size(1)
        trs.right.get.exists(t => t.parent.isEmpty) should be(true)
        assert(w2.balance.get > -20 & w2.balance.get < -19.9 )

        for_cleanup ++= trs.right.get
      }

      "close up one open transactions" in {
        val amount: BigDecimal = 3
        val oreq = OfferRequest.apply(u1.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        for_cleanup ++= trs.right.get

        val w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        assert((w2.balance.get) > -17 & (w2.balance.get) < -16.9 )
        val w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf).right.get
        assert((w1.balance.get) < 17 & (w1.balance.get) > 16.9 )

        trs.isInstanceOf[Right[_,_]] should be(true)
        trs.right.get should have size(1)
        trs.right.get.exists(t => t.parent.isEmpty) should be(false)
//        trs.right.get.forall(_.parent.get.transaction_id == firstPrimordial.transaction_id)
        trs.right.get.exists(t => t.from == u2 && t.to == u2) should be(false)
        trs.right.get.exists(t => t.from == u1 && t.to == u2) should be(false)
//        trs.right.get.filterNot(_.isInstanceOf[BackflowTransaction]).map(_.amount).sum should be(amount)
      }

      "close up two open transactions and create a primordial one" in {
        val amount: BigDecimal = 18
        val oreq = OfferRequest.apply(u1.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u2) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        for_cleanup ++= trs.right.get

        trs.isInstanceOf[Right[_,_]] should be(true)
        trs.right.get should have size(3)
        trs.right.get.count(t => t.parent.isEmpty) should be(1)
        trs.right.get.count(t => t.parent.isDefined) should be(2)
        trs.right.get.exists(t => t.from == u2 && t.to == u2) should be(false)
        trs.right.get.count(t => t.from == u2 && t.to == u1) should be(2)
        trs.right.get.count(t => t.from == u1 && t.to == u2) should be(1)
        trs.right.get.filter(t => t.from == u2 && t.to == u1).forall(_.amount == 0) should be(true)
      }

      "close up three open transactions and backflow one" in {
        val amount: BigDecimal = 17
        val oreq = OfferRequest.apply(u2.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u1) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        for_cleanup ++= trs.right.get

        trs.isInstanceOf[Right[_,_]] should be(true)
        trs.right.get should have size(4)
        trs.right.get.count(t => t.parent.isEmpty) should be(0)
        trs.right.get.count(t => t.parent.isDefined) should be(4)
        trs.right.get.exists(t => t.from == u1 && t.to == u1) should be(true)
        trs.right.get.count(t => t.from == u1 && t.to == u2) should be(3)
        trs.right.get.filterNot(_.isInstanceOf[BackflowTransaction]).map(_.amount).sum should be(amount)
      }

      "cleanup" in {
        val delete_ids = for_cleanup.map(_.transaction_id)
        val q = DbSchema.transactions
        val affected = Await.result(Db.run(q.delete), Duration.Inf)
        affected should be >(0)
      }
    }
  }

}

