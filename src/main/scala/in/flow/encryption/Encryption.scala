package in.flow.encryption

import java.io.{File, FileReader}
import java.security.{KeyPair, PublicKey}
import java.util.Base64
import javax.crypto.Cipher

import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openssl.{PEMDecryptorProvider, PEMEncryptedKeyPair, PEMParser}
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcePEMDecryptorProviderBuilder}

/**
  * All communications with the server must happen through encryption.
  * This object provides the necessary methods to decrypt incoming messages and encrypt outgoing messages.
  */
object Encryption {
  import EncryptionSettings.XFORM

  /** takes a message in base64 format and decrypts it with the server key */
  def receive(message: String): String = {
    ServerEncryption.decrypt(message)
  }

  /** returns a message in base64 format encrypted with the public key */
  def send(message: String, public_key: PublicKey): String = {
    ???
  }
}

private object ServerEncryption {
  import EncryptionSettings.XFORM

  private val key_directory = "encryption-keys/"
  private val settings_file = "es.txt"

  /** Key pair used for all encryption decryption purposes */
  private val server_key_pair = getServerKey()

  /** takes a message in base64 format and decrypts it with the server key */
  private[encryption] def decrypt(message: String): String = {
    val cipher = Cipher.getInstance(XFORM)
    cipher.init(Cipher.DECRYPT_MODE, server_key_pair.getPrivate)
    val decoded_msg = Base64.getDecoder.decode(message)
    val decrypted_bytes = cipher.doFinal(decoded_msg)
    bytesToString(decrypted_bytes)
  }

  private def bytesToString(bytes: Array[Byte] ): String = {
    bytes.map(_.toChar).mkString
  }

  private def getServerKey() = {
    val settings = loadSettings()
    val pem_parser = new PEMParser(new FileReader(settings.private_key_path))
    val key_object = pem_parser.readObject()
    parseKeyPair(key_object, settings.pass)
  }

  /** Reads the text file with pass and key name */
  private def loadSettings(): Settings = {
    val settings_path = key_directory + settings_file
    val settings = scala.io.Source.fromFile(settings_path)
    val lines = settings.getLines().toSeq
    val pass = lines.head
    val key_path = key_directory + lines(1)
    Settings(pass, key_path)
  }

  /** Decrypts the key (making sure it was encrypted with a passphrase), and converts it to java format. */
  private def parseKeyPair(key_object: AnyRef, pass: String): KeyPair = {
    val decryption_provider: PEMDecryptorProvider =
      new JcePEMDecryptorProviderBuilder().setProvider(new BouncyCastleProvider()).build(pass.toCharArray)
    key_object match {
      case ko: PEMEncryptedKeyPair => {
        val decrypted = ko.decryptKeyPair(decryption_provider)
        new JcaPEMKeyConverter().getKeyPair(decrypted)
      }
      case _ => throw new Exception("key is not encrypted")
    }
  }

  private case class Settings(pass: String, private_key_path: String)
}

private object EncryptionSettings {
  private[encryption] val XFORM = "RSA"
}
