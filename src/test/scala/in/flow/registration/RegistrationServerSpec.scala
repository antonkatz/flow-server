package in.flow.registration

import java.util.Base64

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{HttpHeader, StatusCodes}
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import in.flow.db.{Db, DbSchema}
import in.flow.security.{Security, TestVariables}
import in.flow.server.{InnerRoutes, JsonSupport, ServerDirectives}
import in.flow.users.UserAccount
import in.flow.users.registration.Registrar
import org.scalatest.{Matchers, WordSpec}
import spray.json._
import slick.jdbc.PostgresProfile.api._


/**
  * For testing encryption filters on the servlet
  */
class RegistrationServerSpec extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport with
  JsonSupport {
  implicit private val mock_security = new Security(None, None)

  "Servlet" when {
    "during registration" should {
      "register a proper applicant" in {
        // required by Registrar to create an account
        Security.setPublicKey(TestVariables.public_key)

        val ic = Registrar.createInvitation(UserAccount("test_id", "dn")).map(_.code).right.get
        val json_str = "{\"display_name\": \"test\", \"invitation_code\": \"" + ic + "\"}"
          val json = json_str.parseJson
        Post("/register", content = json) ~> TestInnerRoutes.insecureInnerRoute(mock_security) ~> check {
          val r = responseAs[JsValue]
          println(r)

          status should be (StatusCodes.OK)
          val uid = r.asJsObject.fields("response").asJsObject.fields("id").convertTo[String]
          Db.run(DbSchema.user_accounts.filter(_.id === uid).delete)
        }
      }
    }
  }
}

object TestServerDirectives extends ServerDirectives {

  import akka.http.scaladsl.server.Directives.entity

  override def sentity[T](um: FromRequestUnmarshaller[T], s: Security): Directive1[T] = {
    entity(um)
  }
}

object TestInnerRoutes extends InnerRoutes {
  override protected val sd: ServerDirectives = TestServerDirectives
}

