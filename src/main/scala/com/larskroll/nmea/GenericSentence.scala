package com.larskroll.nmea

import fastparse.all._

sealed trait MessageTag {
  def raw(): String;
}

case class StandardTag(talkerId: String, messageId: String) extends MessageTag {
  lazy val raw = talkerId + messageId;
}

case class ProprietaryTag(companyId: String, messageId: String) extends MessageTag {
  lazy val raw = s"P$companyId$messageId";
}

case class GenericSentence(tag: MessageTag, fields: Seq[Option[String]], checksum: Option[Byte]) {
  def isValid(): Boolean = {
    checksum match {
      case Some(target) => {
        var cs: Byte = tag.raw.foldLeft(0.asInstanceOf[Byte])(byteXOR);
        cs = fields.foldLeft(cs)((acc, f) => f match {
          case Some(fi) => fi.foldLeft(byteXOR(acc, ','))(byteXOR);
          case None     => byteXOR(acc, ',')
        });
        cs == target
      }
      case None => true
    }
  }

  private def byteXOR(c1: Char, c2: Char): Byte = (c1.toByte ^ c2.toByte).toByte;
  private def byteXOR(c1: Byte, c2: Char): Byte = (c1 ^ c2.toByte).toByte;
}

object GenericParser {

  def parse(s: String) = sentence.parse(s);

  val reservedChars = ",*\r\n!$\\^~";

  val sentence = P((normalSentence | encapsulatedSentence) ~/ "\r\n");
  val normalSentence: P[GenericSentence] = P("$" ~/ tag ~ "," ~/ fields ~ checksum.?).map {
    case (t, f, c) => GenericSentence(t, f, c)
  };
  val encapsulatedSentence: P[GenericSentence] = P("!" ~/ tag ~ "," ~/ fields ~ checksum.?).map(_ => ???);
  val tag: P[MessageTag] = P(ptag | stag);
  val tagChar = P(CharIn('A' to 'Z'));
  val stag: P[MessageTag] = P(tagChar.rep(exactly = 2).! ~ tagChar.rep(exactly = 3).!).map(p => StandardTag(p._1, p._2));
  val ptag: P[MessageTag] = P("P" ~/ tagChar.rep(exactly = 3).! ~ tagChar.rep.!).map(p => ProprietaryTag(p._1, p._2));
  val strChars = P(CharsWhile(!reservedChars.contains(_: Char)).!);
  val fields = P(strChars.?.rep(sep = ",".~/));
  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'));
  val checksum: P[Byte] = P("*" ~/ hexDigit.rep(exactly = 2).!).map(Integer.parseInt(_, 16).toByte);
}
