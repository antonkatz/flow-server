package in.flow.users

import in.flow.db.{Db, DbSchema}
import in.flow.getLogger
import in.flow.users.registration.{Registrar, RegistrationRequest}
import org.scalatest.WordSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import slick.jdbc.PostgresProfile.api._
import scribe._
/**
  * Created by anton on 14/03/17.
  */
class ConnectionsSpec extends WordSpec {
  private val logger = getLogger("ConSpec")

  "Connections module" when {
    var u: UserAccount = UserAccount("SnbEGywWGtzvGOrtGUVJGVrGL1fA7iKNo4PspuJxh+g=", "connections primordial test",
      in.flow_test.mock_public_key)
//    val ins = Db.db.run(DbSchema.user_accounts += u.storable)
//    Await.ready(ins, Duration.Inf)

    "finding connections" should {
      "find all within a set depth" in {
        val create = true

        if (create) {
          var last_u = u
          (0 to 3) foreach { l =>
            val icode = Registrar.createInvitation(last_u).right.get.code
            val req = RegistrationRequest(icode, s"con.test level $l")
            last_u = Registrar.register(req, Option(in.flow_test.mock_public_key)).right.get
            logger.info(s"Created user at level $l: $last_u")
          }
        }

        u = Await.result[UserAccount](Users.loadUserFully(u), Duration.Inf)

        val cons = Await.result(Connections.getVisibleConnections(u), Duration.Inf)
        logger.info(cons.mkString("\n"))
      }
    }
  }
}
