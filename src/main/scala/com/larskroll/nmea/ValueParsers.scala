package com.larskroll.nmea

import fastparse.all._
import squants.space._
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

  val utcTime = P(utcTime3dig | utcTime2dig | utcTime1dig | utcTime0dig);
  
  private val timeFormat0dig = DateTimeFormatter.ofPattern("HHmmss");
  private val utcTime0dig: P[LocalTime] = P(digit.rep(exactly = 6).!).map {
    case (s) => LocalTime.parse(s, timeFormat0dig)
  };
  
  private val timeFormat1dig = DateTimeFormatter.ofPattern("HHmmss.S");
  private val utcTime1dig: P[LocalTime] = P((digit.rep(exactly = 6) ~ "." ~ digit.rep(exactly = 1)).!).map {
    case (s) => LocalTime.parse(s, timeFormat2dig)
  };
  
  private val timeFormat2dig = DateTimeFormatter.ofPattern("HHmmss.SS");
  private val utcTime2dig: P[LocalTime] = P((digit.rep(exactly = 6) ~ "." ~ digit.rep(exactly = 2)).!).map {
    case (s) => LocalTime.parse(s, timeFormat2dig)
  };
  
  private val timeFormat3dig = DateTimeFormatter.ofPattern("HHmmss.SSS");
  private val utcTime3dig: P[LocalTime] = P((digit.rep(exactly = 6) ~ "." ~ digit.rep(exactly = 3)).!).map {
    case (s) => LocalTime.parse(s, timeFormat3dig)
  };
  
  
  
}
