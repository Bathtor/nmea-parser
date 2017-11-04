package com.larskroll.nmea

import fastparse.all._
import squants.space._
import squants.time.Seconds
import java.time.LocalTime
import java.time.format.DateTimeFormatter

object ValueParsers {
  val digit = P(CharIn('0' to '9'));

  val latitude = P(fullLat | shortLat);
  private val fullLat: P[Angle] = P(digit.rep(exactly = 2).! ~ arcminFrac).map {
    case (d, am) => Degrees(d.toInt) + Arcminutes(am)
  };
  private val shortLat = P(digit.! ~ arcminFrac).map {
    case (d, am) => Degrees(d.toInt) + Arcminutes(am)
  };

  val longitude = P(fullLon | twoLon | oneLon);
  private val fullLon = P(digit.rep(exactly = 3).! ~ arcminFrac).map {
    case (d, am) => Degrees(d.toInt) + Arcminutes(am)
  };
  private val twoLon = P(digit.rep(exactly = 2).! ~ arcminFrac).map {
    case (d, am) => Degrees(d.toInt) + Arcminutes(am)
  };
  private val oneLon = P(digit.! ~ arcminFrac).map {
    case (d, am) => Degrees(d.toInt) + Arcminutes(am)
  };

  private val arcminFrac: P[Double] = P((digit.rep(exactly = 2) ~ "." ~ digit.rep).!).map(s => s.toDouble);

  val utcTime = P(utcTimeNdig | utcTime0dig);

  private val timeFormat = DateTimeFormatter.ofPattern("HHmmss");
  private val utcTime0dig: P[LocalTime] = P(digit.rep(exactly = 6).!).map {
    case (s) => LocalTime.parse(s, timeFormat)
  };

  private val utcTimeNdig: P[LocalTime] = P(digit.rep(exactly = 6).! ~ "." ~/ digit.rep.!).map {
    case (hms, s) => {
      val hmsLT = LocalTime.parse(hms, timeFormat);
      val seconds = Seconds(s"0.$s".toDouble);
      hmsLT.plusNanos(seconds.toNanoseconds.toLong)
    }
  };

}
