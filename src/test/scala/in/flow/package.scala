package in

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

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
}
