package com.lunatech.urldecode

import java.lang.IllegalArgumentException
import scala.io.Codec

object Decoder {

  /**
   * Match either the start of the string, which does not start with a percent sign, or
   * a percent sign followed by other characters.
   */
  val splitAtPercentRE = "^[^%]+|%[^%]*".r
  def partitionByPercent(s: String) = splitAtPercentRE.findAllIn(s)

  /**
   * URL decode percent-encoding
   * @param str percent-encoded String
   * @return Decoded String value
   *
   * Strategy:
   *
   * - Partition the input into strings that start with the percent sign,
   *   possibly except for the first one.
   * - For each substring, if it starts with percent, then test that it's followed by
   *   at least 2 other characters, then take those, interpret as hex and produce a byte.
   *   Append the rest of the string as bytes
   * - If the substring didn't start with a percent, just convert it into bytes. All
   *   byte conversions use UTF-8 encoding
   * - We now have a sequence of sequences of bytes. flattening will give us a sequence of
   *   bytes. Then going through Array[Byte] we can go back to String.
   */
  def decode(str: String): String = {
    val partitionedStrings: Seq[String] = partitionByPercent(str).toSeq
    val byteSeqs: Seq[Array[Byte]] = partitionedStrings.map { s =>
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
