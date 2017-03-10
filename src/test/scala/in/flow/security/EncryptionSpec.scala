package in.flow.security

import java.util.Base64

import in.flow.utils.Hex
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
        val decrypted = Encryption.receive(Base64.getDecoder.decode(message))
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

      "succeed with another key" in {
        val key = Encryption.parsePublicKey("-----BEGIN PUBLIC KEY-----\nMIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvq7+LdGjlYwk4y8iov2xgSZElxrFNbpkJguDJVFwJDFh+GcXZOENOQ8LOEKAHFq9pLm5hxZspjE9EajT9SlDmLu89Vq82r1Rls5crPG3c2eIPlb3+IBG22WG63Sbl71AKYk90CXXmfmCfEkydacznSBpzuoUGpt7VUH5XIe4NRPT6oJqcK7GN2ZO5csguMhj+5vcNIWC3+x3oCEMrm1WB4ifU8CU4Tdf0tZ/n2rVCe0H/F42wBAgyH3Nfv3XPMpo2h9CbUpmaANARS0gIXSbrSrPwAPhrQIElJ0eWU/YAqLVjdczr6PPdiQYo1HLhb8IZFU9eanYRC+HGHjooq0AOaHbPUywzxNHwmmC/Eh17PdBIvwJe0rTy4JLDz8mDz5mQpbeB8hMCyOk9mHFu8cJW0digUadNTZMFHkMSq6pGGfdAzH7CGf7IZ5oXYt1wGVBqO2VRx2zOuoSh5cPyQozuzzJTOGCXCzOBvUXc25RvAhcHCokLVkEWOY/p+sYRmBn8IOsUXqjW437WUUsCRwQOLXe0VrofdvCtwmIEk4oriSGo6zrbFC3vqf0GmQuDpdjprM3dnXevfDWejAGWl06dBHLTGQx6kWNGkxKw5mDikVF+f/6+8g8nVetNEH34qzVduPk9+LY09T0dxaGuXoSRPCMi9MxK/SUFEPiiW02R18CAwEAAQ==\n-----END PUBLIC KEY-----")
        key.get
      }
    }

    "dealing with symmetric encryption" should {
      "be able to generate a key" in {
        val key = Encryption.generateSymmetricKey
        assert(key.getEncoded.length == 32)
      }
    }

    "binary data from javascript" should {
      "be decoded" in {
        val encrypted_bytes = Hex.parse("b930d71c90401dbd8b8dc77a68c6114bccd0e032860d0ab5b143eb3f458bf2c9")
        val key_bytes = Hex.parse("0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")
        val iv = Hex.parse("2122232425262728292a2b2c2d2e2f30")

        val plaintext =
          Encryption.parseSymmetricKey(key_bytes).map(secret => Encryption.receive(encrypted_bytes, secret, iv)).get
        plaintext === "TextMustBe16Byte"
        println(plaintext)
      }

      "be encoded" in {
        val plaintext = "test - of"
        val key_bytes = Hex.parse("0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")

        val enc1 = Encryption.parseSymmetricKey(key_bytes).map { secret => Encryption.send(plaintext, secret) }.get
        assert(enc1.iv.length == 16)
        assert(enc1.message.length > 0)
        val enc2 = Encryption.parseSymmetricKey(key_bytes).map { secret => Encryption.send(plaintext, secret) }.get
        //        the results of the second run should be different from the first
        assert {
          !(enc1.iv sameElements enc2.iv)
        }
        assert {
          !(enc1.message sameElements enc2.message)
        }

        println(enc1.message.toList)
        println(enc2.message.toList)
      }
    }
  }
}
