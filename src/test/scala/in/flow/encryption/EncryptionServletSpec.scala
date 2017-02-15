package in.flow.encryption

import java.security.PublicKey

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

        "decrypt through a filter" in {
          val body_encrypted = "ORLeMFga4sFPGU6CnzsPUMJpjurK0RAFhy1Crln/VYbMxS3jrfe1vVe1C2sm+ivolZ2nocTms1ng\nfE0eh3j6xj9I7CLzsSCOBX6jMnqb34GwQa7QsQ4DbBYiIofejhLvDsafaJLuNkLkNXbaGRT0KENF\nAkELICSGfga+IlsUkkwzSohbYcEtO2M3hWN9+Y+bF2XXB4PdrzwsFeEBGFfd265UgYP4EEDdF9dw\n+UejMNgFb3PzuOSxnZI5lYX5Ml04fAE1MTHJETPqImdMcSOulelLkts4XMrW/ArF5o8cvF+RJkee\n9HbnPPbYlQESCN+xOvPNyh65202V+eZ+Fp87ZBPrkCbOpWbNG2HtQ60dXoIIMJLqjwTH10bI24Rt\nOakJ6bCvDu5J1DDruHIhTC4KdRjr2ohSUBJ1oVZBF8R45+ui5b0MoQktw0F9Mc7XhGP5CIW+v4Th\nm8WNHNHUWNv1leLBVKcBQLUAZGlEusizDLYNre+q2x3RsPzTj5HpIT4pnuGEVfZxaidNNRRFSucB\naNdYkTcG9+4yXKL7ur9urgs/x8c23WfXaaVuNu/4PdisyDMd1NKKkCukn9RHm5iImDnwHCpxiY+i\nWK/NqMu2Th6m379Us6CBWsDi8xXcUcidTnx/xdmhiSUg78GdJ70Hulw17zsCI7baA8MGBVRoKWE="
          post("/decryption", body = body_encrypted) {
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
  override protected val production: Boolean = true

  post("/decryption") {
    if (parsedBody == TestVariables.incoming_body_json) Ok() else halt(400)
  }

  get("/encryption") {
    TestVariables.outgoing_body_plain
  }

  override protected[flow] def getSendersPublicKey: PublicKey = Encryption.getServerPublicKey
}