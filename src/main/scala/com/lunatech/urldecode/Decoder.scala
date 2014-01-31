package com.lunatech.urldecode

import java.nio.charset.Charset
import java.lang.IllegalArgumentException
import scala.io.Codec

object Decoder {

  val splitAtPercentRE = "^[^%]+|%[^%]*".r
  def partitionByPercent(s: String) = splitAtPercentRE.findAllIn(s)

  /**
   * URL decode percent-encoding
   * @param str percent-encoded String
   * @return Decoded String value
   */
  def decode(str: String): String = {
    val partitionedStrings = partitionByPercent(str).toSeq
    val byteSeqs = partitionedStrings.map { s =>
      if (s.startsWith("%"))
        if (s.length < 3)
          throw new IllegalArgumentException("% not followed by 2 characters")
        else
          hexToByte(s.substring(1,3)) +: s.substring(3).getBytes(Codec.UTF8.charSet)
      else
        s.getBytes(Codec.UTF8.charSet)
    }
    new String(byteSeqs.flatten.toArray, Codec.UTF8.charSet)
  }

  def hexToByte(s: String): Byte = {
    try {
      Integer.parseInt(s, 16).toByte
    } catch {
      case e: Exception => throw new IllegalArgumentException("Illegal hex characters");
    }
  }
}
