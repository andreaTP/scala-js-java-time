package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.ChronoField

import utest._
import org.scalajs.testsuite.javalib.time.TemporalAccessorTest

object DayOfWeekTestTemporalAccess extends TestSuite with TemporalAccessorTest[DayOfWeek] {
  import DayOfWeek._

  val samples = values.toSeq

  def isSupported(field: ChronoField): Boolean =
    field == ChronoField.DAY_OF_WEEK

  val tests = temporalAccessorTests
}

object DayOfWeekTest extends TestSuite {
  import DayOfWeek._

  val samples = values.toSeq

  val tests = Tests {
    'test_getValue - {
      assert(1 == MONDAY.getValue)
      assert(2 == TUESDAY.getValue)
      assert(3 == WEDNESDAY.getValue)
      assert(4 == THURSDAY.getValue)
      assert(5 == FRIDAY.getValue)
      assert(6 == SATURDAY.getValue)
      assert(7 == SUNDAY.getValue)
    }

    'test_getLong - {
      for (d <- samples)
        assert(d.getValue.toLong == d.getLong(ChronoField.DAY_OF_WEEK))
    }

    'test_plus - {
      assert(FRIDAY == SATURDAY.plus(Long.MinValue))
      assert(THURSDAY == THURSDAY.plus(-7))
      assert(SUNDAY == WEDNESDAY.plus(-3))
      assert(MONDAY == TUESDAY.plus(-1))
      assert(MONDAY == MONDAY.plus(0))
      assert(FRIDAY == THURSDAY.plus(1))
      assert(MONDAY == FRIDAY.plus(3))
      assert(SATURDAY == SATURDAY.plus(7))
      assert(SUNDAY == SUNDAY.plus(Long.MaxValue))
    }

    'test_minus - {
      assert(SUNDAY == SATURDAY.minus(Long.MinValue))
      assert(THURSDAY == THURSDAY.minus(-7))
      assert(MONDAY == FRIDAY.minus(-3))
      assert(WEDNESDAY == TUESDAY.minus(-1))
      assert(MONDAY == MONDAY.minus(0))
      assert(WEDNESDAY == THURSDAY.minus(1))
      assert(SUNDAY == WEDNESDAY.minus(3))
      assert(SATURDAY == SATURDAY.minus(7))
      assert(SUNDAY == SUNDAY.plus(Long.MaxValue))
    }

    'test_compareTo - {
      assert(0 == WEDNESDAY.compareTo(WEDNESDAY))
      assert(MONDAY.compareTo(SUNDAY) < 0)
      assert(SATURDAY.compareTo(TUESDAY) > 0)
    }

    'test_values - {
      val days =
        Seq(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)
      assert(days == values.toSeq)
    }

    'test_valueOf - {
      assert(MONDAY == valueOf("MONDAY"))
      assert(TUESDAY == valueOf("TUESDAY"))
      assert(WEDNESDAY == valueOf("WEDNESDAY"))
      assert(THURSDAY == valueOf("THURSDAY"))
      assert(FRIDAY == valueOf("FRIDAY"))
      assert(SATURDAY == valueOf("SATURDAY"))
      assert(SUNDAY == valueOf("SUNDAY"))

      intercept[IllegalArgumentException](valueOf(""))
    }

    'test_of - {
      assert(MONDAY == of(1))
      assert(TUESDAY == of(2))
      assert(WEDNESDAY == of(3))
      assert(THURSDAY == of(4))
      assert(FRIDAY == of(5))
      assert(SATURDAY == of(6))
      assert(SUNDAY == of(7))

      for (n <- Seq(Int.MinValue, 0, 8, Int.MaxValue))
        intercept[DateTimeException](of(n))
    }

    'test_from - {
      for (d <- samples)
        assert(d == from(d))
      for (d <- Seq(LocalDate.MIN, LocalDate.of(2012, 2, 29), LocalDate.MAX))
        assert(d.getDayOfWeek == from(d))

      intercept[DateTimeException](from(LocalTime.MIN))
      intercept[DateTimeException](from(Month.JANUARY))
    }
  }
}
