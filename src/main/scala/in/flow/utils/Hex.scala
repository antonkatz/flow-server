package in.flow.utils

/**
  * Created by anton on 16/02/17.
  */
object Hex {
  def parse(buf: Array[Byte]): String = buf.map("%02X" format _).mkString

  def parse(hex: String): Array[Byte] = {
    if(hex.contains(" ")){
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if(hex.contains("-")){
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2,2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }
}
