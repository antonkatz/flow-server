package in.flow.users

import in.flow.commformats.ExternalCommFormats.OfferRequest
import in.flow.commformats.InternalCommFormats._
import in.flow.db.{Db, DbSchema}
import in.flow.{getLogger, _}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import scribe.Logger
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


/**
  * For testing encryption filters on the servlet
  */
class UsersSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  private val logger: Logger = "UsersSpec"

  var u1: UserAccount = UserAccount("u1", "what a friend", in.flow_test.mock_public_key)
  var u2: UserAccount = UserAccount("u2", "you call", in.flow_test.mock_public_key)
  var u3: UserAccount = UserAccount("u3", "connector", in.flow_test.mock_public_key)

  "Users" when {
    "performing transactions" should {
      "have the setup users unconnected directly" in {
        logger.info("running setup")
        val ins = Db.run(DBIO.seq(DbSchema.user_accounts += u1.storable, DbSchema.user_accounts += u2.storable,
          DbSchema.user_accounts += u3.storable))
        Await.ready(ins, Duration.Inf)

        Await.ready(Connections.connectUsers(u1, u3, UserConnectionType.friend), Duration.Inf)
        Await.ready(Connections.connectUsers(u2, u3, UserConnectionType.friend), Duration.Inf)

        u1 = Await.result(Users.loadUserConnections(u1), Duration.Inf)
        u2 = Await.result(Users.loadUserConnections(u2), Duration.Inf)

        (!u1.connections.exists(_._1 == u2)) should be (true)
        (!u2.connections.exists(_._1 == u1)) should be (true)
      }

      "connect two users if not already connected" in {
        logger.info("running connection on transaction")
        val amount: BigDecimal = 10
        val oreq = OfferRequest.apply(u2.user_id, amount, None)
        val trsf = Offers.createOffer(oreq, u1) flowWith Users.performTransaction
        val trs = Await.result(trsf, Duration.Inf)

        // because the connective code executes onComplete
        Thread.sleep(100)

        u1 = Await.result(Users.reloadUserConnections(u1), Duration.Inf)
        u2 = Await.result(Users.reloadUserConnections(u2), Duration.Inf)
        (u1.connections.exists(_._1 == u2)) should be (true)
        (u2.connections.exists(_._1 == u1)) should be (true)
      }
    }
  }

  override def afterAll() = {
    val uset = Set(u1, u2, u3).map(_.user_id)
    val q_remove_cons = DbSchema.user_account_connections.filter(con => (con.from inSet uset) || (con.to inSet uset))
    Await.ready(Db.run(q_remove_cons.delete), Duration.Inf)
  }
}

