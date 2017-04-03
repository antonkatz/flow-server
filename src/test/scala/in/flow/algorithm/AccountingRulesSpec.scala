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


    "calculating interest" should {
      var firstPrimordial: Transaction = null
      "calculate 0 for a wallet with negative balance AND a non-zero with a wallet with positive balance" in {
        val amount: BigDecimal = 10
        val oreq = OfferRequest(u2.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u1) flowWith Wallet.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        Thread.sleep(1000)

        val w1 = Await.result(Wallet.getWallet(u1) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)
        val w2 = Await.result(Wallet.getWallet(u2) flowRight Wallet.loadAuxWalletInfo, Duration.Inf)

        w1.right.get.uncommitted_interest should be (Some(0))
        val w2_ui: BigDecimal = w2.right.get.uncommitted_interest.get
        assert(w2_ui > 0)
        for_cleanup ++= trs.right.get
      }
    }
  }

  override def afterAll(): Unit = {
      val delete_ids = for_cleanup.map(_.transaction_id)
      val q = DbSchema.transactions.filter(t => t.transactionId inSet delete_ids)
      val affected = Await.result(Db.run(q.delete), Duration.Inf)
      affected should be >(0)
  }
}

