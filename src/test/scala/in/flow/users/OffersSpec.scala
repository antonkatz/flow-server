package in.flow.users

import in.flow.commformats.ExternalCommFormats.OfferRequest
import in.flow.commformats.InternalCommFormats.UserConnectionType
import in.flow.users.UserAccount
import in.flow.db.{Db, DbSchema}
import in.flow.getLogger
import org.scalatest.{Matchers, WordSpec}
import scribe.Logger
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


/**
  * For testing encryption filters on the servlet
  */
class OffersSpec extends WordSpec with Matchers {
  private val logger: Logger = "OfferSpec"
  "Offers" when {
//    val offering_to = Await.result(Users.lazyGetUser("uGfcjvhzihkjLR8D1LiFCqbBE2DkiKfk6vhRGafXZk0="), Duration.Inf).get
    val offering_to = Await.result(Users.lazyGetUser("vJamK1drGiodBvw7ekXAQPpMBWe81koPd7ULKQv1nUM="), Duration.Inf).get

    val u1: UserAccount = UserAccount("friend1", "what a friend", in.flow_test.mock_public_key)
    val u2: UserAccount = UserAccount("friend2", "you call", in.flow_test.mock_public_key)
    val ins = Db.run(DBIO.seq(DbSchema.user_accounts += u1.storable, DbSchema.user_accounts += u2.storable))
    Await.ready(ins, Duration.Inf)

    Connections.connectUsers(offering_to, u1, UserConnectionType.friend)
    Connections.connectUsers(offering_to, u2, UserConnectionType.friend)

    "creating on offer" should {
      "do so for external testing" in {
        val f1 = Users.loadUserFully(u1) flatMap { u =>
          Offers.createOffer(OfferRequest(offering_to.user_id, 0.5, Option("let me help write you a song")), u)
        }
        val f2 = Users.loadUserFully(u2) flatMap {u =>
          Offers.createOffer(OfferRequest(offering_to.user_id, 2.4, Option("a very long and detailed message of " +
            "the heaben and the erathe sot oent lorem ipsum onvmt eoaset edioasm arfun wot eorkb ienrsfm")), u)
        }
        val res = Await.result(Future.sequence(Seq(f1, f2)), Duration.Inf)
        logger.debug("completed setup for external tests")
        logger.debug(res)
      }

      // todo. find better word
      "fail when the amount is too precise" in {
        val f = Users.loadUserFully(u2) flatMap { u =>
          Offers.createOffer(OfferRequest(offering_to.user_id, 0.55, Option("i had to write something very long" +
            "and something very complicated cause im a dick")), u)
        }
        f onComplete {res =>
          res.get.isInstanceOf[Left[_,_]] should be(true)
        }
        Await.ready(f, Duration.Inf)
      }
    }
  }
}

