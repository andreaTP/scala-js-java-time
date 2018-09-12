package org.scalajs.testsuite.javalib.time

import java.time.DateTimeException
import java.time.temporal.{ChronoUnit, Temporal, UnsupportedTemporalTypeException}

import utest._

object DateTimeTestUtil {

  val dateBasedUnits = ChronoUnit.values.filter(_.isDateBased)

  val timeBasedUnits = ChronoUnit.values.filter(_.isTimeBased)

  def testDateTime(actual: => Any)(expected: => Any): Unit = {
    try {
      val e = expected
      //expectNoException(actual)
      assert(e == actual)
    } catch {
      case _: UnsupportedTemporalTypeException =>
        intercept[UnsupportedTemporalTypeException](actual)

      case _: DateTimeException =>
        intercept[DateTimeException](actual)

      case _: ArithmeticException =>
        intercept[ArithmeticException](actual)
    }
  }
}
