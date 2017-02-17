package in.flow.security

import java.security.PublicKey
import java.util.Base64

import in.flow.server.FlowServlet
import org.scalatest.WordSpec
import org.scalatra.Ok
import org.scalatra.test.scalatest._

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
          get("/encryption") {
            val decrypted_body = Encryption.receive(body)
            decrypted_body should equal(TestVariables.outgoing_body_plain)
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

  get("/encryption") {
    TestVariables.outgoing_body_plain
  }

  override protected[flow] def getSendersPublicKey: PublicKey = Encryption.getServerPublicKey
}