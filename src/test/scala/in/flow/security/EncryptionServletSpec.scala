package in.flow.security

import java.util.Base64

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import org.bouncycastle.util.encoders.Hex
import org.scalatest.{Matchers, WordSpec}
import spray.json._

/**
  * For testing encryption filters on the servlet
  */
class EncryptionServletSpec extends WordSpec with Matchers with ScalatestRouteTest {

  "Servlet" when {
    "receiving" should {

      val hpk = HttpHeader.parse("public_key", Base64.getEncoder.encodeToString(TestVariables.public_key.getBytes))
        .asInstanceOf[ParsingResult.Ok].header

      "decrypt through a filter" in {
        val bodyEncrypted = Base64.getEncoder.encodeToString(Encryption.send(TestVariables.incoming_body_plain,
          Encryption.getServerPublicKey))
        val hf = HttpHeader.parse("format", "base64").asInstanceOf[ParsingResult.Ok].header

        Post("/decryption", content = bodyEncrypted).withHeaders(hf, hpk).mapEntity(_.withContentType(ContentTypes
          .`application/json`)) ~> TestServlet.route ~> check {
          status shouldBe StatusCodes.OK
        }
      }

      "not change the flag on error" in {
        val bodyEncrypted = Base64.getEncoder.encodeToString(Encryption.send(TestVariables.incoming_body_plain,
          Encryption.getServerPublicKey))
        val hf = HttpHeader.parse("format", "base64").asInstanceOf[ParsingResult.Ok].header
        val hiv = HttpHeader.parse("iv", "").asInstanceOf[ParsingResult.Ok].header

        Post("/decryption/flag", content = bodyEncrypted).withHeaders(hf, hpk, hiv)
          .mapEntity(_.withContentType(ContentTypes.`application/json`)) ~> TestServlet.route ~> check {
          println(status)
          println(responseAs[String])
          TestVariables.trigger_flag shouldBe false

        }
      }

      "encrypt through a filter" in {
          Post("/encryption").withHeaders(hpk) ~> TestServlet.route ~> check {
            status shouldBe StatusCodes.OK

            val dsk = Encryption.receiveAsBytes(Hex.decode(response.getHeader("key").get().value()))
            val symmetric_key = Encryption.parseSymmetricKey(Hex.decode(dsk)).get
            val (iv, body) = responseAs[ByteString].toArray.splitAt(16)
            val decrypted_body = Encryption.receive(body, symmetric_key, iv)

            new String(decrypted_body) should equal(TestVariables.outgoing_body_plain)
          }
      }

      "replying" should {
        //        "notify if it cannot decrypt the payload" in {
        //          val iv = new Array[Byte](16)
        //          Random.nextBytes(iv)
        //          post("/fail-decryption", body="something", headers = Map("iv" -> Hex.toHexString(iv))) {
        //              status should be (421)
        //          }
      }
    }
  }
}

object TestVariables {
  var trigger_flag = false

  val incoming_body_plain = "{\"value\":\"test\"}"
  val incoming_body_json = incoming_body_plain.parseJson
  val outgoing_body_plain = "test"
  val public_key = "-----BEGIN PUBLIC KEY-----\nMIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvq7+LdGjlYwk4y8iov2x\ngSZElxrFNbpkJguDJVFwJDFh+GcXZOENOQ8LOEKAHFq9pLm5hxZspjE9EajT9SlD\nmLu89Vq82r1Rls5crPG3c2eIPlb3+IBG22WG63Sbl71AKYk90CXXmfmCfEkydacz\nnSBpzuoUGpt7VUH5XIe4NRPT6oJqcK7GN2ZO5csguMhj+5vcNIWC3+x3oCEMrm1W\nB4ifU8CU4Tdf0tZ/n2rVCe0H/F42wBAgyH3Nfv3XPMpo2h9CbUpmaANARS0gIXSb\nrSrPwAPhrQIElJ0eWU/YAqLVjdczr6PPdiQYo1HLhb8IZFU9eanYRC+HGHjooq0A\nOaHbPUywzxNHwmmC/Eh17PdBIvwJe0rTy4JLDz8mDz5mQpbeB8hMCyOk9mHFu8cJ\nW0digUadNTZMFHkMSq6pGGfdAzH7CGf7IZ5oXYt1wGVBqO2VRx2zOuoSh5cPyQoz\nuzzJTOGCXCzOBvUXc25RvAhcHCokLVkEWOY/p+sYRmBn8IOsUXqjW437WUUsCRwQ\nOLXe0VrofdvCtwmIEk4oriSGo6zrbFC3vqf0GmQuDpdjprM3dnXevfDWejAGWl06\ndBHLTGQx6kWNGkxKw5mDikVF+f/6+8g8nVetNEH34qzVduPk9+LY09T0dxaGuXoS\nRPCMi9MxK/SUFEPiiW02R18CAwEAAQ==\n-----END PUBLIC KEY-----"
}

private[security] object TestServlet extends DefaultJsonProtocol with SprayJsonSupport {

  import StatusCode.int2StatusCode
  import in.flow.server.ServerDirectives._

  val route = securityDirective { implicit security =>
    implicit val s = security

    (secureRequestDirective(security) & secureResponseDirective(security) & securityRejectionHandler(security)) {
      post {
            pathPrefix("decryption") {
              println("/decryption")

              path("flag") {
                println("/decryption/flag")

                sentity(as[String], s) { _ =>
                  println("changing flag")
                  TestVariables.trigger_flag = !TestVariables.trigger_flag
                  complete(StatusCodes.OK)
                }
              } ~ pathEndOrSingleSlash {
                println("/decryption [catchall]")
                sentity(as[JsValue], s) { body =>
                  if (body == TestVariables.incoming_body_json)
                    complete(int2StatusCode(200))
                  else complete(int2StatusCode(400))
                }
              }

            } ~ path("encryption") {
              complete(TestVariables.outgoing_body_plain)
            }

      }
    }
  }

  //  post("/encryption") {
  //    println("@ /ENCRYPTION")
  //    val public_key = Base64.getDecoder.decode(request.getHeader("pk")).map(_.toChar).mkString
  //    Security.setPublicKey(public_key)
  //    TestVariables.outgoing_body_plain
  //  }
  //
  //  post("/fail-decryption") {
  //    println("@ /fail-decryption")
  //  }
}