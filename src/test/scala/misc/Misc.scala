package misc

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by anton on 14/02/17.
  */
class Misc extends WordSpec with Matchers {
  /** Trying to firuge out how to write a partial function for Unit*/

  def partialFunction: PartialFunction[Any, String] = {
    case unit: Unit => ""
    case _: Unit | Unit | null => ""
    case other => "other"
  }

  "Function" when {
    "called" should {
      "identify Unit correctly" in {
        partialFunction(Unit) === ""
        partialFunction(() => Unit) === ""
      }
    }
  }
}
