package in

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import in.flow.security.Security
import in.flow.server.{InnerRoutes, ServerDirectives}

import scala.util.Random

/**
  * Created by anton on 13/03/17.
  */
package object flow_test {
  val keyGen = KeyPairGenerator.getInstance("RSA")
  keyGen.initialize(1024)

  def mock_public_key: PublicKey = {
    val key = keyGen.generateKeyPair()
    key.getPublic
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
}
