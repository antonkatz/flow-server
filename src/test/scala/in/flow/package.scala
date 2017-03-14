package in

import java.security.PublicKey

/**
  * Created by anton on 13/03/17.
  */
package object flow_test {
  val mock_public_key = new PublicKey {
    override def getEncoded = Array[Byte]()
    override def getFormat = ???
    override def getAlgorithm = ???
  }
}
