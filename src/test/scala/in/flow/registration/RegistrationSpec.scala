package in.flow.registration

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model.DateTime
import in.flow.db.{Db, DbSchema}
import in.flow.security.Encryption
import in.flow.users.UserAccount
import in.flow.users.registration.{Invitation, Registrar, RegistrationRequest}
import org.scalatest.{Matchers, WordSpec}
import scribe.formatter.{Formatter, FormatterBuilder}
import slick.ast.JoinType
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util
import scala.util.Left
//import com.typesafe.slick
import scribe._

/**
  * Created by anton on 06/03/17.
  */
class RegistrationSpec extends WordSpec with Matchers {
  "Registration module" when {

    val u: UserAccount = UserAccount("test_id", "desired test name")
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

    "registering a new user" should {
      "succeed" in {
        val i = Registrar.createInvitation(u).right.get

        val r = RegistrationRequest(i.code, "user desired name")
        val new_u = Registrar.register(r, Option(Encryption.getServerPublicKey))

        val qi= DbSchema.invitations.filter(_.code === i.code).exists.result
        val qdu = DbSchema.user_accounts.filter(_.id === new_u.map(_.user_id).getOrElse("")).delete

        new_u.isRight should be(true)
        Await.result(Db.run(qi), Duration.Inf) should be(false)
        // deleting the new user, since the pub key is always the same
        Await.result(Db.run(qdu), Duration.Inf)
      }
    }
  }
}
