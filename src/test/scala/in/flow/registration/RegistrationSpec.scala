package in.flow.registration

import java.util.concurrent.TimeUnit

import in.flow.db.{Db, DbSchema}
import in.flow.users.UserAccount
import in.flow.users.registration.{Invitation, Registrar}
import org.scalatest.{Matchers, WordSpec}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Left
//import com.typesafe.slick

/**
  * Created by anton on 06/03/17.
  */
class RegistrationSpec extends WordSpec with Matchers {
  "Registration module" when {

    val u: UserAccount = UserAccount("test_id")
    val ins = Db.db.run(DbSchema.user_accounts += u)
    Await.ready(ins, Duration.Inf)

    "creating an invitation code" should {
      "succeed" in {
        val i = Registrar.createInvitation(u)
        i shouldBe a [Right[_, Invitation]]

        val q = DbSchema.invitations.filter(_.code === i.right.get.code).result
        val qr = Await.result(Db.db.run( q ), Duration.Inf)

        qr.head.code should not be empty
      }
    }
  }
}
