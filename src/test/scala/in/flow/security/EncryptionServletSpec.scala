package in.flow.security

import java.security.PublicKey
import java.util.Base64

import in.flow.server.FlowServlet
import org.bouncycastle.util.encoders.Hex
import org.scalatest.WordSpec
import org.scalatra.Ok
import org.scalatra.test.scalatest._

import scala.util.Random

/**
  * For testing encryption filters on the servlet
  */
class EncryptionServletSpec extends WordSpec with ScalatraSuite {
    // `HelloWorldServlet` is your app which extends ScalatraServlet
    addServlet(classOf[ProductionTestServlet], "/*")

    "Servlet" when {
      "receiving" should {
        val body_encrypted = "ORLeMFga4sFPGU6CnzsPUMJpjurK0RAFhy1Crln/VYbMxS3jrfe1vVe1C2sm+ivolZ2nocTms1ngfE0eh3j6xj9I7CLzsSCOBX6jMnqb34GwQa7QsQ4DbBYiIofejhLvDsafaJLuNkLkNXbaGRT0KENFAkELICSGfga+IlsUkkwzSohbYcEtO2M3hWN9+Y+bF2XXB4PdrzwsFeEBGFfd265UgYP4EEDdF9dw+UejMNgFb3PzuOSxnZI5lYX5Ml04fAE1MTHJETPqImdMcSOulelLkts4XMrW/ArF5o8cvF+RJkee9HbnPPbYlQESCN+xOvPNyh65202V+eZ+Fp87ZBPrkCbOpWbNG2HtQ60dXoIIMJLqjwTH10bI24RtOakJ6bCvDu5J1DDruHIhTC4KdRjr2ohSUBJ1oVZBF8R45+ui5b0MoQktw0F9Mc7XhGP5CIW+v4Thm8WNHNHUWNv1leLBVKcBQLUAZGlEusizDLYNre+q2x3RsPzTj5HpIT4pnuGEVfZxaidNNRRFSucBaNdYkTcG9+4yXKL7ur9urgs/x8c23WfXaaVuNu/4PdisyDMd1NKKkCukn9RHm5iImDnwHCpxiY+iWK/NqMu2Th6m379Us6CBWsDi8xXcUcidTnx/xdmhiSUg78GdJ70Hulw17zsCI7baA8MGBVRoKWE="

        "have a functional base64 decoder" in {
          val decoded = Base64.getDecoder.decode(body_encrypted)
        }

        "decrypt through a filter" in {

          post("/decryption", body = body_encrypted, headers = List(("encoding_format", "base64"))) {
            status should equal(200)
            val decrypted_body = Encryption.receive(body)
            decrypted_body should be(empty)
          }
        }

        "encrypt through a filter" in {
          val pk = "-----BEGIN PUBLIC KEY-----\nMIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvq7+LdGjlYwk4y8iov2x\ngSZElxrFNbpkJguDJVFwJDFh+GcXZOENOQ8LOEKAHFq9pLm5hxZspjE9EajT9SlD\nmLu89Vq82r1Rls5crPG3c2eIPlb3+IBG22WG63Sbl71AKYk90CXXmfmCfEkydacz\nnSBpzuoUGpt7VUH5XIe4NRPT6oJqcK7GN2ZO5csguMhj+5vcNIWC3+x3oCEMrm1W\nB4ifU8CU4Tdf0tZ/n2rVCe0H/F42wBAgyH3Nfv3XPMpo2h9CbUpmaANARS0gIXSb\nrSrPwAPhrQIElJ0eWU/YAqLVjdczr6PPdiQYo1HLhb8IZFU9eanYRC+HGHjooq0A\nOaHbPUywzxNHwmmC/Eh17PdBIvwJe0rTy4JLDz8mDz5mQpbeB8hMCyOk9mHFu8cJ\nW0digUadNTZMFHkMSq6pGGfdAzH7CGf7IZ5oXYt1wGVBqO2VRx2zOuoSh5cPyQoz\nuzzJTOGCXCzOBvUXc25RvAhcHCokLVkEWOY/p+sYRmBn8IOsUXqjW437WUUsCRwQ\nOLXe0VrofdvCtwmIEk4oriSGo6zrbFC3vqf0GmQuDpdjprM3dnXevfDWejAGWl06\ndBHLTGQx6kWNGkxKw5mDikVF+f/6+8g8nVetNEH34qzVduPk9+LY09T0dxaGuXoS\nRPCMi9MxK/SUFEPiiW02R18CAwEAAQ==\n-----END PUBLIC KEY-----"
          val h: Iterable[(String, String)] = Map("pk" -> Base64.getEncoder.encodeToString(pk))
          post(uri = "/encryption", body = "", headers = h) {
            val iv = Hex.decode(response.getHeader("iv"))
            val dsk = Encryption.receiveAsBytes(Hex.decode(response.getHeader("key")))
            val symmetric_key = Encryption.parseSymmetricKey(Hex.decode(dsk)).get

            status should be (200)
            val decrypted_body = Encryption.receive(bodyBytes, symmetric_key, iv)
            new String(decrypted_body) should equal(TestVariables.outgoing_body_plain)
          }
        }
      }

      "replying" should {
        "notify if it cannot decrypt the payload" in {
          val iv = new Array[Byte](16)
          Random.nextBytes(iv)
          post("/fail-decryption", body="something", headers = Map("iv" -> Hex.toHexString(iv))) {
              status should be (421)
          }
        }
      }
    }
}

object TestVariables {
  import org.json4s._
  import org.json4s.jackson.JsonMethods._

  val incoming_body_plain = "{\"value\":\"test\"}"
  val incoming_body_json = parse(incoming_body_plain)
  val outgoing_body_plain = "test"
}

class ProductionTestServlet extends FlowServlet {
  override protected val decrypt_incoming: Boolean = true

  post("/decryption") {
    println(parsedBody)
    if (parsedBody == TestVariables.incoming_body_json) Ok() else halt(400)
  }

  post("/encryption") {
    println("@ /ENCRYPTION")
    val public_key = Base64.getDecoder.decode(request.getHeader("pk")).map(_.toChar).mkString
    Security.setPublicKey(public_key)
    TestVariables.outgoing_body_plain
  }

  post("/fail-decryption") {
    println("@ /fail-decryption")
  }

  override protected[flow] def getSendersPublicKey: PublicKey = Encryption.getServerPublicKey
}