package in.flow.registration

import java.util.Base64

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import akka.util.ByteString
import in.flow.security.{Encryption, Security, TestVariables}
import org.bouncycastle.util.encoders.Hex
import org.scalatest.{Matchers, WordSpec}
import spray.json._
import in.flow.server.{InnerRoutes, ServerDirectives}

/**
  * For testing encryption filters on the servlet
  */
class RegistrationServerSpec extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {
  private val mock_security = new Security(None, None)

  "Servlet" when {
    "during registration" should {
      "register a proper applicant" in {
        val json = "{\"name\": \"test\", \"invitation_code\": \"test_code\"}".parseJson
        Post("/register", content = json) ~> TestInnerRoutes.insecureInnerRoute(mock_security) ~> check {
          println(responseAs[String])
        }
      }
    }
  }
}

object TestServerDirectives extends ServerDirectives {
  import akka.http.scaladsl.server.Directives.entity

  override def sentity[T](um: FromRequestUnmarshaller[T], s: Security): Directive1[T] = entity(um)
}

object TestInnerRoutes extends InnerRoutes {
  override protected val sd: ServerDirectives = TestServerDirectives
}

