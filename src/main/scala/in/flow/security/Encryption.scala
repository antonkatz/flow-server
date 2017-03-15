package in.flow.security

import java.io.{FileReader, StringReader}
import java.security.spec.RSAPublicKeySpec
import java.security.{KeyFactory, KeyFactorySpi, KeyPair, PublicKey}
import java.util.Base64
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{Cipher, KeyGenerator, SecretKey}

import org.bouncycastle.asn1._
import org.bouncycastle.asn1.pkcs.{PKCSObjectIdentifiers, RSAPublicKey}
import org.bouncycastle.asn1.x509.{AlgorithmIdentifier, SubjectPublicKeyInfo}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcePEMDecryptorProviderBuilder}
import org.bouncycastle.openssl.{PEMDecryptorProvider, PEMEncryptedKeyPair, PEMParser}
import sun.security.rsa.RSAKeyFactory

import scala.util.Try

/**
  * All communications with the server must happen through encryption.
  * This object provides the necessary methods to decrypt incoming messages and encrypt outgoing messages.
  */
object Encryption {
  java.security.Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())

  /** takes a message as bytes format and decrypts it with the server key */
  def receive(message: Array[Byte]): String = {
    getString(receiveAsBytes(message))
  }

  /** takes asymmetrically encrypted message, and returns decrypted bytes */
  def receiveAsBytes(message: Array[Byte]): Array[Byte] = AsymmetricEncryption.decrypt(message)

  /** takes symmetrically encrypted message, and returns decrypted bytes */
  def receive(message: Array[Byte], key: SecretKey, iv: Array[Byte]): Array[Byte] = {
    SymmetricEncryption.receive(message, key, iv)
  }

  /** returns a message (parsed as UTF-8) as bytes encrypted with the public key or asymmetrical cipher */
  def send(message: String, public_key: PublicKey): Array[Byte] =
    AsymmetricEncryption.send(getBytes(message), public_key)

  /** returns a message (parsed as UTF-8) as bytes encrypted with the public key or asymmetrical cipher */
  def send(message: Array[Byte], public_key: PublicKey): Array[Byte] =
    AsymmetricEncryption.send(message, public_key)

  /** returns a message as bytes encrypted with a symmetrical cipher */
  def send(message: String, secret_key: SecretKey): SymmetricallyEncrypted =
    SymmetricEncryption.send(getBytes(message), secret_key)

  /** returns a message as bytes encrypted with a symmetrical cipher */
  def send(message: Array[Byte], secret_key: SecretKey): SymmetricallyEncrypted =
    SymmetricEncryption.send(message, secret_key)

  /** uses a pem parser (requires the BEGIN/END directive) */
  def parsePublicKey(key_string: String): Option[PublicKey] = AsymmetricEncryption.parsePublicKey(key_string)

  def parsePublicKey(bytes: Array[Byte]): Option[PublicKey] = AsymmetricEncryption.parsePublicKey(bytes)

  def generateSymmetricKey: SecretKey = {
    val generator = KeyGenerator.getInstance(EncryptionSettings.symmetric_cipher_key_type)
    generator.init(EncryptionSettings.symmetric_cipher_key_size)
    generator.generateKey()
  }

  def parseSymmetricKey(bytes: Array[Byte]): Option[SecretKey] = SymmetricEncryption.parseKeyFromBytes(bytes)

  def getServerPublicKey: PublicKey = AsymmetricEncryption.getServerPublicKey

  private[security] def getString(bytes: Array[Byte]): String = {
    new String(bytes, EncryptionSettings.string_encoding)
  }

  private[security] def getBytes(msg: String) = msg.getBytes(EncryptionSettings.string_encoding)
}

private object SymmetricEncryption {
  /** returns a (message, iv) tuple */
  private[security] def send(message: Array[Byte], key: SecretKey): SymmetricallyEncrypted = {
    val cipher = Cipher.getInstance(EncryptionSettings.symmetric_cipher_type)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    val params = cipher.getParameters
    val iv = params.getParameterSpec(classOf[IvParameterSpec]).getIV
    SymmetricallyEncrypted(cipher.doFinal(message), iv)
  }

  private[security] def receive(message: Array[Byte], key: SecretKey, iv: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance(EncryptionSettings.symmetric_cipher_type)
    cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
    cipher.doFinal(message)
  }

  private[security] def parseKeyFromBytes(key_bytes: Array[Byte]): Option[SecretKey] = try {
    Some(new SecretKeySpec(key_bytes, EncryptionSettings.symmetric_cipher_key_type))
  } catch {
    case _: Throwable => None
  }
}

case class SymmetricallyEncrypted(message: Array[Byte], iv: Array[Byte])

import scribe._

private object AsymmetricEncryption {
  private val logger = "AsymEncryption".logger

  import Encryption.getString
  import EncryptionSettings.asymmetric_cipher_type

  private val key_directory = "encryption-keys/"
  private val settings_file = "es.txt"

  /** Key pair used for all encryption decryption purposes */
  private val server_key_pair = getServerKey

  private[security] def getServerPublicKey = server_key_pair.getPublic

  /** takes a message in base64 format and decrypts it with the server key */
  private[security] def decrypt(message: String): Array[Byte] = {
    val decoded_msg = Base64.getDecoder.decode(message)
    decrypt(decoded_msg)
  }

  /** takes a message as bytes and decrypts it with the server key */
  private[security] def decrypt(message: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance(asymmetric_cipher_type)
    cipher.init(Cipher.DECRYPT_MODE, server_key_pair.getPrivate)
    cipher.doFinal(message)
  }

  /** takes a message and encodes it with the given key */
  private[security] def send(message: Array[Byte], public_key: PublicKey): Array[Byte] = {
    val cipher = Cipher.getInstance(asymmetric_cipher_type)
    cipher.init(Cipher.ENCRYPT_MODE, public_key)
    cipher.doFinal(message)
  }

  /** same as the other send, returning base64 string instead of bytes */
  private[security] def sendAsString(message: Array[Byte], public_key: PublicKey): String = {
    val bytes = Base64.getEncoder.encode(send(message, public_key))
    getString(bytes)
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


  /** uses a pem parser (requires the BEGIN/END directive) */
  private[security] def parsePublicKey(key_string: String): Option[PublicKey] = {
    val pem_parser = new PEMParser(new StringReader(key_string))
    val key_object = pem_parser.readObject()
    checkAndGetAsPublic.lift(key_object)
  }

  private[security] def parsePublicKey(bytes: Array[Byte]): Option[PublicKey] = Try {
    val s = ASN1Sequence.getInstance(bytes)
    val dls = s.getObjectAt(1).toASN1Primitive.asInstanceOf[DERBitString].getBytes
    val rsaPubStructure = RSAPublicKey.getInstance(dls)
    val spki = new SubjectPublicKeyInfo(new AlgorithmIdentifier(PKCSObjectIdentifiers.rsaEncryption, DERNull.INSTANCE),
      rsaPubStructure)
    checkAndGetAsPublic.lift(spki)
  } recover {
    case e => logger.error(s"Could not parse public key from bytes: ${e.getMessage}"); None
  } getOrElse None

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

  private def checkAndGetAsPublic: PartialFunction[Object, PublicKey] = {
    case ko: SubjectPublicKeyInfo => new JcaPEMKeyConverter().getPublicKey(ko)
  }

  private case class Settings(pass: String, private_key_path: String)
}

private object EncryptionSettings {
  private[security] val asymmetric_cipher_type = "RSA"
  private[security] val symmetric_cipher_key_type = "AES"
  private[security] val symmetric_cipher_type = "AES/CBC/PKCS7Padding"
  private[security] val symmetric_cipher_key_size = 256
  private[security] val string_encoding = in.flow.global_string_format
}
