package org.scalajs.testsuite.javalib.time.chrono

import java.time.{DateTimeException, LocalTime, LocalDate}
import java.time.chrono.ChronoLocalDate

import utest._

class ChronoLocalDateTest extends TestSuite {
  import ChronoLocalDate._

  val tests = Tests {

    'test_timeLineOrder - {
      val ord = timeLineOrder
      val ds = Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX)

      for {
        d1 <- ds
        d2 <- ds
      } {
        assert(math.signum(d1.compareTo(d2)) == math.signum(ord.compare(d1, d2)))
      }
    }

    'test_from - {
      for (d <- Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX))
        assert(d == from(d))

      for (t <- Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX))
        intercept[DateTimeException](from(t))
    }
  }
}
