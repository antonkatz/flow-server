package in.flow.users

import in.flow.algorithm.AlgorithmSettings
import in.flow.commformats.RegistrationRequest
import in.flow.getLogger
import in.flow.users.registration.Registrar
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Created by anton on 14/03/17.
  */
class ConnectionsSpec extends WordSpec with Matchers {
  private val logger = getLogger("ConSpec")

  "Connections module" when {
    var u: UserAccount = UserAccount("SnbEGywWGtzvGOrtGUVJGVrGL1fA7iKNo4PspuJxh+g=", "connections primordial test",
      in.flow_test.mock_public_key)

    "finding connections" should {
      "find all within a set depth" in {
        val create = false

        if (create) {
          var last_u = u
          (0 to 3) foreach { l =>
            val icode = Registrar.createInvitation(last_u).right.get.code
            val req = RegistrationRequest(icode, s"con.test level $l")
            last_u = Registrar.register(req, Option(in.flow_test.mock_public_key)).right.get
            logger.info(s"Created user at level $l: $last_u")
          }
        }

        // todo. this test should take into account last_u as well
        // currently the tests go from creator -> end
        // but what about end -> creator

        u = Await.result[UserAccount](Users.loadUserFully(u), Duration.Inf)

        val cons = Await.result(Connections.getVisibleConnections(u), Duration.Inf)
        logger.info(cons.mkString("\n"))

        for (i <- 0 until AlgorithmSettings.connections_search_depth) {
          val cl = cons(i)
          val correct_level = cl forall {
            _.display_name.endsWith(i.toString)
          }
          correct_level should be(true)
        }
        cons should have length (AlgorithmSettings.connections_search_depth)
      }
    }
  }
}
