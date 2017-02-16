package in.flow.security

import java.io.{File, FileReader, StringReader}
import java.security.{KeyPair, PublicKey}
import java.util.Base64
import javax.crypto.{Cipher, KeyGenerator, SecretKey}

import org.bouncycastle.asn1.pkcs.RSAPublicKey
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openssl.{PEMDecryptorProvider, PEMEncryptedKeyPair, PEMParser}
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcePEMDecryptorProviderBuilder}

/**
  * All communications with the server must happen through encryption.
  * This object provides the necessary methods to decrypt incoming messages and encrypt outgoing messages.
  */
object Encryption {
  import EncryptionSettings.asymmetric_cipher_type

  /** takes a message in base64 format and decrypts it with the server key */
  def receive(message: String): String = {
    AsymmetricEncryption.decrypt(message)
  }

  /** returns a message as bytes encrypted with the public key or a symmetrical cipher */
  def send(message: String, public_key: PublicKey): String = {
    val cipher = Cipher.getInstance(asymmetric_cipher_type)
    cipher.init(Cipher.ENCRYPT_MODE, public_key)
    val encoded = cipher.doFinal(message.getBytes)
    val bytes = Base64.getEncoder.encode(encoded)
    bytesToString(bytes)
  }

  def parsePublicKey(key_string: String): Option[PublicKey] = {
    def checkAndGetAsPublic: PartialFunction[Object, PublicKey] = {
      case ko: SubjectPublicKeyInfo => new JcaPEMKeyConverter().getPublicKey(ko)
    }

    val pem_parser = new PEMParser(new StringReader(key_string))
    val key_object = pem_parser.readObject()
    checkAndGetAsPublic.lift(key_object)
  }

  def generateSymmetricKey: SecretKey = {
    val generator = KeyGenerator.getInstance(EncryptionSettings.symmetric_cipher_key_type)
    generator.init(EncryptionSettings.symmetric_cipher_key_size)
    generator.generateKey()
  }

  def getServerPublicKey = AsymmetricEncryption.getServerPublicKey

  private[security] def bytesToString(bytes: Array[Byte] ): String = {
    bytes.map(_.toChar).mkString
  }
}

private object AsymmetricEncryption {
  import EncryptionSettings.asymmetric_cipher_type
  import Encryption.bytesToString

  private val key_directory = "encryption-keys/"
  private val settings_file = "es.txt"

  /** Key pair used for all encryption decryption purposes */
  private val server_key_pair = getServerKey

  private[security] def getServerPublicKey = server_key_pair.getPublic

  /** takes a message in base64 format and decrypts it with the server key */
  private[security] def decrypt(message: String): String = {
    val decoded_msg = Base64.getDecoder.decode(message)
    decrypt(decoded_msg)
  }

  /** takes a message as bytes and decrypts it with the server key */
  private[security] def decrypt(message: Array[Byte]): String = {
    val cipher = Cipher.getInstance(asymmetric_cipher_type)
    cipher.init(Cipher.DECRYPT_MODE, server_key_pair.getPrivate)
    val decrypted_bytes = cipher.doFinal(message)
    bytesToString(decrypted_bytes)
  }

  /** takes a message and encodes it with the given key */
  private[security] def send(message: String, public_key: PublicKey): Array[Byte] = {
    val cipher = Cipher.getInstance(asymmetric_cipher_type)
    cipher.init(Cipher.ENCRYPT_MODE, public_key)
    cipher.doFinal(message.getBytes)
  }

  /** same as the other send, returning base64 string instead of bytes */
  private[security] def sendAsString(message: String, public_key: PublicKey): String = {
    val bytes = Base64.getEncoder.encode(send(message, public_key))
    bytesToString(bytes)
  }

  private def getServerKey = {
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
  private[security] val asymmetric_cipher_type = "RSA"
  private[security] val symmetric_cipher_key_type = "AES"
  private[security] val symmetric_cipher_type = "AES/CBC"
  private[security] val symmetric_cipher_key_size = 256
}
