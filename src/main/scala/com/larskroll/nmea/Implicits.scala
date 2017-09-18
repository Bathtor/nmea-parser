package com.larskroll.nmea

import fastparse.all.Parsed
import Parsed.{ Success, Failure }
import com.typesafe.scalalogging.Logger

object Implicits {
  implicit class ParsedOptionable[A](p: Parsed[A]) {
    def toOption: Option[A] = p match {
      case Success(r, _) => Some(r)
      case _: Failure    => None
    }

    def toOption(logger: Logger): Option[A] = p match {
      case Success(r, _) => Some(r)
      case fe: Failure => {
        logger.warn("Issue occurred while parsing {} at index {}. Error was:\n{}", fe.extra.input, fe.index, fe.extra.traced.trace);
        None
      }
    }
  }
}
