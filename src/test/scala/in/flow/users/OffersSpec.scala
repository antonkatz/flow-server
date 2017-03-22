package in.flow.users

import in.flow.commformats.OfferRequest
import in.flow.db.{Db, DbSchema}
import in.flow.getLogger
import org.scalatest.{Matchers, WordSpec}
import scribe.Logger
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


/**
  * For testing encryption filters on the servlet
  */
class OffersSpec extends WordSpec with Matchers {
  private val logger: Logger = "OfferSpec"
  "Offers" when {
    val offering_to = Await.result(Users.lazyGetUser("uGfcjvhzihkjLR8D1LiFCqbBE2DkiKfk6vhRGafXZk0="), Duration.Inf).get

    val u1: UserAccount = UserAccount("friend1", "what a friend", in.flow_test.mock_public_key)
    val u2: UserAccount = UserAccount("friend2", "you call", in.flow_test.mock_public_key)
    val ins = Db.db.run(DBIO.seq(DbSchema.user_accounts += u1.storable, DbSchema.user_accounts += u2.storable))
    Await.ready(ins, Duration.Inf)

    Connections.connectUsers(offering_to.user_id, u1.user_id, UserConnectionType.friend)
    Connections.connectUsers(offering_to.user_id, u2.user_id, UserConnectionType.friend)

    "creating on offer" should {
      "do so for external testing" in {
        val f = Users.loadUserFully(u2) flatMap { u =>
          Offers.createOffer(OfferRequest(offering_to.user_id, 1, Option("i had to write something very long" +
            "and something very complicated cause im a dick")), u)
        }
        f onComplete {res =>
          logger.info(res)
          logger.info("finished setup for external tests")
        }
        Await.ready(f, Duration.Inf)
      }
    }
  }
}

