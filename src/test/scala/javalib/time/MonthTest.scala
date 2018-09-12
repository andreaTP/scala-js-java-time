package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.ChronoField

import utest._

object MonthTestTemporalAccessor extends TestSuite with TemporalAccessorTest[Month] {
import Month._

  val samples = values.toSeq

  def isSupported(field: ChronoField): Boolean =
    field == ChronoField.MONTH_OF_YEAR

  val tests = temporalAccessorTests
}

object MonthTest extends TestSuite {
  import Month._

  val samples = values.toSeq

  val tests = Tests {

    'test_getValue - {
      assert(1 == JANUARY.getValue)
      assert(2 == FEBRUARY.getValue)
      assert(3 == MARCH.getValue)
      assert(4 == APRIL.getValue)
      assert(5 == MAY.getValue)
      assert(6 == JUNE.getValue)
      assert(7 == JULY.getValue)
      assert(8 == AUGUST.getValue)
      assert(9 == SEPTEMBER.getValue)
      assert(10 == OCTOBER.getValue)
      assert(11 == NOVEMBER.getValue)
      assert(12 == DECEMBER.getValue)
    }

    'test_getLong - {
      for (m <- samples)
        assert(m.getValue.toLong == m.getLong(ChronoField.MONTH_OF_YEAR))
    }

    'test_plus - {
      assert(MAY == JANUARY.plus(Long.MinValue))
      assert(FEBRUARY == FEBRUARY.plus(-12))
      assert(FEBRUARY == MARCH.plus(-1))
      assert(APRIL == APRIL.plus(0))
      assert(JUNE == MAY.plus(1))
      assert(JUNE == JUNE.plus(12))
      assert(FEBRUARY == JULY.plus(Long.MaxValue))
    }

    'test_minus - {
      assert(SEPTEMBER == JANUARY.minus(Long.MinValue))
      assert(FEBRUARY == FEBRUARY.minus(-12))
      assert(APRIL == MARCH.minus(-1))
      assert(APRIL == APRIL.minus(0))
      assert(APRIL == MAY.minus(1))
      assert(JUNE == JUNE.minus(12))
      assert(DECEMBER == JULY.minus(Long.MaxValue))
    }

    'test_minLength - {
      assert(31 == JANUARY.minLength)
      assert(28 == FEBRUARY.minLength)
      assert(31 == MARCH.minLength)
      assert(30 == APRIL.minLength)
      assert(31 == MAY.minLength)
      assert(30 == JUNE.minLength)
      assert(31 == JULY.minLength)
      assert(31 == AUGUST.minLength)
      assert(30 == SEPTEMBER.minLength)
      assert(31 == OCTOBER.minLength)
      assert(30 == NOVEMBER.minLength)
      assert(31 == DECEMBER.minLength)
    }

    'test_maxLength - {
      assert(31 == JANUARY.maxLength)
      assert(29 == FEBRUARY.maxLength)
      assert(31 == MARCH.maxLength)
      assert(30 == APRIL.maxLength)
      assert(31 == MAY.maxLength)
      assert(30 == JUNE.maxLength)
      assert(31 == JULY.maxLength)
      assert(31 == AUGUST.maxLength)
      assert(30 == SEPTEMBER.maxLength)
      assert(31 == OCTOBER.maxLength)
      assert(30 == NOVEMBER.maxLength)
      assert(31 == DECEMBER.maxLength)
    }

    'test_length - {
      for (m <- samples) {
        assert(m.minLength == m.length(false))
        assert(m.maxLength == m.length(true))
      }
    }

    'test_firstDayOfYear - {
      assert(1 == JANUARY.firstDayOfYear(false))
      assert(1 == JANUARY.firstDayOfYear(true))
      assert(32 == FEBRUARY.firstDayOfYear(false))
      assert(32 == FEBRUARY.firstDayOfYear(true))
      assert(60 == MARCH.firstDayOfYear(false))
      assert(61 == MARCH.firstDayOfYear(true))
      assert(91 == APRIL.firstDayOfYear(false))
      assert(92 == APRIL.firstDayOfYear(true))
      assert(121 == MAY.firstDayOfYear(false))
      assert(122 == MAY.firstDayOfYear(true))
      assert(152 == JUNE.firstDayOfYear(false))
      assert(153 == JUNE.firstDayOfYear(true))
      assert(182 == JULY.firstDayOfYear(false))
      assert(183 == JULY.firstDayOfYear(true))
      assert(213 == AUGUST.firstDayOfYear(false))
      assert(214 == AUGUST.firstDayOfYear(true))
      assert(244 == SEPTEMBER.firstDayOfYear(false))
      assert(245 == SEPTEMBER.firstDayOfYear(true))
      assert(274 == OCTOBER.firstDayOfYear(false))
      assert(275 == OCTOBER.firstDayOfYear(true))
      assert(305 == NOVEMBER.firstDayOfYear(false))
      assert(306 == NOVEMBER.firstDayOfYear(true))
      assert(335 == DECEMBER.firstDayOfYear(false))
      assert(336 == DECEMBER.firstDayOfYear(true))
    }

    'test_firstMonthOfQuarter - {
      assert(JANUARY == JANUARY.firstMonthOfQuarter)
      assert(JANUARY == FEBRUARY.firstMonthOfQuarter)
      assert(JANUARY == MARCH.firstMonthOfQuarter)
      assert(APRIL == APRIL.firstMonthOfQuarter)
      assert(APRIL == MAY.firstMonthOfQuarter)
      assert(APRIL == JUNE.firstMonthOfQuarter)
      assert(JULY == JULY.firstMonthOfQuarter)
      assert(JULY == AUGUST.firstMonthOfQuarter)
      assert(JULY == SEPTEMBER.firstMonthOfQuarter)
      assert(OCTOBER == OCTOBER.firstMonthOfQuarter)
      assert(OCTOBER == NOVEMBER.firstMonthOfQuarter)
      assert(OCTOBER == DECEMBER.firstMonthOfQuarter)
    }

    'test_compareTo - {
      assert(0 == JULY.compareTo(JULY))
      assert(JANUARY.compareTo(MARCH) < 0)
      assert(DECEMBER.compareTo(OCTOBER) > 0)
    }

    'test_values - {
      val months = Seq(JANUARY, FEBRUARY, MARCH, APRIL, MAY,
          JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER)
      assert(months == values.toSeq)
    }

    'test_valueOf - {
      assert(JANUARY == valueOf("JANUARY"))
      assert(FEBRUARY == valueOf("FEBRUARY"))
      assert(MARCH == valueOf("MARCH"))
      assert(APRIL == valueOf("APRIL"))
      assert(MAY == valueOf("MAY"))
      assert(JUNE == valueOf("JUNE"))
      assert(JULY == valueOf("JULY"))
      assert(AUGUST == valueOf("AUGUST"))
      assert(SEPTEMBER == valueOf("SEPTEMBER"))
      assert(OCTOBER == valueOf("OCTOBER"))
      assert(NOVEMBER == valueOf("NOVEMBER"))
      assert(DECEMBER == valueOf("DECEMBER"))

      intercept[IllegalArgumentException](valueOf(""))
    }

    'test_of - {
      assert(JANUARY == of(1))
      assert(FEBRUARY == of(2))
      assert(MARCH == of(3))
      assert(APRIL == of(4))
      assert(MAY == of(5))
      assert(JUNE == of(6))
      assert(JULY == of(7))
      assert(AUGUST == of(8))
      assert(SEPTEMBER == of(9))
      assert(OCTOBER == of(10))
      assert(NOVEMBER == of(11))
      assert(DECEMBER == of(12))

      for (n <- Seq(Int.MinValue, 0, 13, Int.MaxValue))
        intercept[DateTimeException](of(n))
    }

    'test_from - {
      for (m <- samples) {
        assert(m == from(m))
        assert(m == from(LocalDate.of(1, m, 1)))
      }

      intercept[DateTimeException](from(DayOfWeek.MONDAY))
      intercept[DateTimeException](from(LocalTime.MIN))
    }
  }
}
