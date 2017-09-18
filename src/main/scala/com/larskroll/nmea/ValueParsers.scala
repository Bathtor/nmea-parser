package com.larskroll.nmea

import fastparse.all._
import squants.space._

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
}
