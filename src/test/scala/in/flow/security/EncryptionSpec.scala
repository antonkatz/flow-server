package in.flow.security

import java.security.Security
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import org.scalatest.WordSpec

/**
  * Created by anton on 12/02/17.
  */
class EncryptionSpec extends WordSpec {
  "Encryption module" when {
    val test_message = "the message"

    "asked to decrypt and incoming message" should {
      "return the test message" in {
        val message = "i9XU9uglgIzV+1YZ+27R5RG/CUPmpRda7kQQavlp1LABh0cxylsyvcbqGigKHZhJ2tH8S6ImU2WIB0mAqphPRoPkdGCCm3DM75G8G02bl4gI/x+l338SHhizWMNZGACQRkvDdrt62fmuUx897qMrRLHzateQwpQ7Nk1sCELpdu0wzRoIpsUkZz/9LE2Fj6iDqV9o4RkARbTHj50LHv072F1BjIfnXWWKrkZWb8Ld2JEkr4lJO8ziWJxA2Vs0ESuNA7GrlGiWFfj7WvYJHaJC7Ilkeen2uJANTjjQsuvBULW12VfqDH8inO86QLUb01QVOC6pWD+24VvdCNCmgux8YkUbBcgY4BrFaDWqnj0PyROFaJoRnny9Z16jAB+PMtKQwdoGH9WM3bBw/zVLLSg/V3A670eYrXrqODvkLI4wg7ZJHgBNHtDOx3FeS6iwewy3j9vnsQ1dsFFo1ynKrU/Oy7bJ4QyF9RW3WmJTLGevsbB669EetiJLPzMicjLO7YD7tzVyinNZFhJEJ5jWp1SSZhMHTUEzEpqtHDWKlh/s2lRYUT54gfxAzoTb7MMt9qE/1M/VJPfwYXdGkMozsLMjrNffg1IL38jLsun8CFybhoxXMmj4Bo10RXsgmMY87/4ohp35Pm707KY6DF9H+HvyJBJccnDxgBUo7zWoDMzb0Mo="
        val decrypted = Encryption.receive(message)
        decrypted === test_message
      }
    }

    "asked to parse a public key" should {
      val key_string = "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvq7+LdGjlYwk4y8iov2x\ngSZElxrFNbpkJguDJVFwJDFh+GcXZOENOQ8LOEKAHFq9pLm5hxZspjE9EajT9SlD\nmLu89Vq82r1Rls5crPG3c2eIPlb3+IBG22WG63Sbl71AKYk90CXXmfmCfEkydacz\nnSBpzuoUGpt7VUH5XIe4NRPT6oJqcK7GN2ZO5csguMhj+5vcNIWC3+x3oCEMrm1W\nB4ifU8CU4Tdf0tZ/n2rVCe0H/F42wBAgyH3Nfv3XPMpo2h9CbUpmaANARS0gIXSb\nrSrPwAPhrQIElJ0eWU/YAqLVjdczr6PPdiQYo1HLhb8IZFU9eanYRC+HGHjooq0A\nOaHbPUywzxNHwmmC/Eh17PdBIvwJe0rTy4JLDz8mDz5mQpbeB8hMCyOk9mHFu8cJ\nW0digUadNTZMFHkMSq6pGGfdAzH7CGf7IZ5oXYt1wGVBqO2VRx2zOuoSh5cPyQoz\nuzzJTOGCXCzOBvUXc25RvAhcHCokLVkEWOY/p+sYRmBn8IOsUXqjW437WUUsCRwQ\nOLXe0VrofdvCtwmIEk4oriSGo6zrbFC3vqf0GmQuDpdjprM3dnXevfDWejAGWl06\ndBHLTGQx6kWNGkxKw5mDikVF+f/6+8g8nVetNEH34qzVduPk9+LY09T0dxaGuXoS\nRPCMi9MxK/SUFEPiiW02R18CAwEAAQ=="
      val full_key_string = "-----BEGIN PUBLIC KEY-----\n" + key_string + "\n-----END PUBLIC KEY-----"
      "succeed with full key" in {
        val key = Encryption.parsePublicKey(full_key_string)
        key.get
      }
      "fail with trimmed key" in {
        val key = Encryption.parsePublicKey(key_string)
        assert(key.isEmpty)
      }
    }

    "dealing with symmetric encryption" should {
      "be able to generate a key" in {
        val key = Encryption.generateSymmetricKey
        assert(key.getEncoded.length == 32)
      }
    }

    "binary data from javascript" should {
      Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
      "be decoded" in {
        //        val encrypted_bytes =
//          Array(220, 154, 34, 203, 242, 134, 72, 173, 214, 102, 200, 146, 91, 145, 24, 73).map(_.asInstanceOf[Byte])
//        val eb_test = encrypted_bytes.map(_.toInt)
        val maxKeyLen = Cipher.getMaxAllowedKeyLength("AES");
        val encrypted_bytes = hex2bytes("b930d71c90401dbd8b8dc77a68c6114bccd0e032860d0ab5b143eb3f458bf2c9")
        val key_bytes = hex2bytes("0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")
//        val key_bytes = (1 to 32).toArray.map(_.toByte)
        val iv = hex2bytes("2122232425262728292a2b2c2d2e2f30")
//        val iv = (33 until (33+16)).toArray.map(_.toByte)

        val secret = new SecretKeySpec(key_bytes, "AES")
        val cipher = Cipher.getInstance("AES/CBC/PKCS7Padding")
//        val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
        cipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(iv))
        val plain_bytes = cipher.doFinal(encrypted_bytes)
        val plaintext = new String(plain_bytes, "UTF-8")
        println(plaintext)
      }

      "be encoded" in {
        val plaintext = "test - of"
        val key_bytes = hex2bytes("0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")
        val iv = hex2bytes("2122232425262728292a2b2c2d2e2f30")

        val algo = "AES/CBC/PKCS7Padding"
        val secret = new SecretKeySpec(key_bytes, algo)
        val cipher = Cipher.getInstance(algo)
        cipher.init(Cipher.ENCRYPT_MODE, secret, new IvParameterSpec(iv))
        val plain_bytes = cipher.doFinal(plaintext.getBytes("UTF-8"))
          .map(_ & 0xFF)
        println(plain_bytes.toList)
      }
    }
  }

  def hex2bytes(hex: String): Array[Byte] = {
    if(hex.contains(" ")){
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if(hex.contains("-")){
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2,2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }
}
