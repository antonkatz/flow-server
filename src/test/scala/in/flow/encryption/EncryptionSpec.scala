package in.flow.encryption

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
  }
}
