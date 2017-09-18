package com.larskroll.nmea

import org.scalatest._
import fastparse.all._

class ExampleSpec extends FlatSpec with Matchers {
  import Parsed.{ Success, Failure };

  "The Parser" should "validate a GGA message" in {
    val msg = "$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47\r\n";
    val res = GenericParser.sentence.parse(msg) match {
      case Success(r, _) => r
      case fe: Failure   => println(s"Error for message at index ${fe.index} (${msg.charAt(fe.index)}):\n${fe.extra.traced.trace}"); null
    }
    res should equal (GenericSentence(StandardTag("GP", "GGA"),
      Seq(Some("123519"), Some("4807.038"), Some("N"), Some("01131.000"),
        Some("E"), Some("1"), Some("08"), Some("0.9"), Some("545.4"), Some("M"),
        Some("46.9"), Some("M"), None, None),
      Some(0x47)));
    res.isValid() should be (true);
  }

  it should "validate a AAM message" in {
    val msg = "$GPAAM,A,A,0.10,N,WPTNME*32\r\n";
    val res = GenericParser.sentence.parse(msg) match {
      case Success(r, _) => r
      case fe: Failure   => println(s"Error for message at index ${fe.index} (${msg.charAt(fe.index)}):\n${fe.extra.traced.trace}"); null
    }
    res should equal (GenericSentence(StandardTag("GP", "AAM"),
      Seq(Some("A"), Some("A"), Some("0.10"), Some("N"), Some("WPTNME")),
      Some(0x32)));
    res.isValid() should be (true);
  }

  it should "validate field contents" in {
    val fields = Seq("123519", "4807.038", "N");
    val res = fields.map(f => {
      GenericParser.strChars.parse(f) match {
        case Success(r, _) => r
        case fe: Failure   => println(s"Error for $f at index ${fe.index}:\n${fe.extra.traced.trace}"); null
      }
    })
    val fieldsExpected = Seq("123519", "4807.038", "N");
    res should equal (fieldsExpected);
  }

  it should "validate fields' contents" in {
    val fields = "123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,";
    val res = GenericParser.fields.parse(fields) match {
      case Success(r, _) => r.toList
      case fe: Failure   => println(s"Error for fields at index ${fe.index} (${fields.charAt(fe.index)}):\n${fe.extra.traced.trace}"); null
    }

    val fieldsExpected = Seq(Some("123519"), Some("4807.038"), Some("N"), Some("01131.000"),
      Some("E"), Some("1"), Some("08"), Some("0.9"), Some("545.4"), Some("M"),
      Some("46.9"), Some("M"), None, None);
    res should equal (fieldsExpected);
  }

  it should "validate tags" in {
    val fields = Seq("GPGGA", "PRAYA", "GPGSA");
    val res = fields.map(f => {
      GenericParser.tag.parse(f) match {
        case Success(r, _) => r
        case fe: Failure   => println(s"Error for $f at index ${fe.index}:\n${fe.extra.traced.trace}"); null
      }
    })
    val fieldsExpected = Seq(StandardTag("GP", "GGA"), ProprietaryTag("RAY", "A"), StandardTag("GP", "GSA"));
    res should equal (fieldsExpected);
  }

  "ValueParsers" should "handle latitude" in {
    import squants.space._

    val lats = Seq("5509.725", "0512.12", "532.1234");
    val res = lats.map(l => {
      ValueParsers.latitude.parse(l) match {
        case Success(r, _) => r
        case fe: Failure   => println(s"Error for $l at index ${fe.index}:\n${fe.extra.traced.trace}"); null
      }
    });
    val latsExpected = Seq((Degrees(55) + Arcminutes(9.725)), (Degrees(5) + Arcminutes(12.12)), (Degrees(5) + Arcminutes(32.1234)));
    res.map(_.toDegrees) should equal (latsExpected.map(_.toDegrees));
  }

  it should "handle longitude" in {
    import squants.space._

    val lons = Seq("00734.221", "17034.21", "0734.1234", "734.1234");
    val res = lons.map(l => {
      ValueParsers.longitude.parse(l) match {
        case Success(r, _) => r
        case fe: Failure   => println(s"Error for $l at index ${fe.index}:\n${fe.extra.traced.trace}"); null
      }
    });
    val lonsExpected = Seq((Degrees(7) + Arcminutes(34.221)), (Degrees(170) + Arcminutes(34.21)), (Degrees(7) + Arcminutes(34.1234)), (Degrees(7) + Arcminutes(34.1234)));
    res.map(_.toDegrees) should equal (lonsExpected.map(_.toDegrees));
  }
}
