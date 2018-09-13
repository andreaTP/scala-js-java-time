package org.scalajs.testsuite.javalib.time

import java.time.chrono.IsoChronology
import java.time.temporal.{TemporalField, UnsupportedTemporalTypeException, ValueRange, ChronoField}
import java.time.{DateTimeException, LocalDate, Month, MonthDay}

import utest._

object MonthDayTestUtils {
  import ChronoField._

  final val min = MonthDay.of(Month.JANUARY, 1)
  final val max = MonthDay.of(Month.DECEMBER, 31)
  final val leapMonth = MonthDay.of(Month.FEBRUARY, 29)

  val yearSamples = 2000 to 2020
  def leapYear(year: Int): Boolean = IsoChronology.INSTANCE.isLeapYear(year)

}

/** Created by alonsodomin on 22/12/2015. */
object MonthDayTestTemporalAccessor extends TestSuite with TemporalAccessorTest[MonthDay] {
  import ChronoField._
  import MonthDayTestUtils._

  val samples = Month.values().map(month => (month, month.minLength(), month.maxLength())).flatMap {
    case (month, minDay, maxDay) =>
      if (minDay != maxDay) Seq(MonthDay.of(month, 1), MonthDay.of(month, minDay), MonthDay.of(month, maxDay))
      else Seq(MonthDay.of(month, 1), MonthDay.of(month, minDay))
  }.toSeq

  override def isSupported(field: ChronoField): Boolean =
    field == ChronoField.MONTH_OF_YEAR || field == ChronoField.DAY_OF_MONTH

  override def expectedRangeFor(accessor: MonthDay, field: TemporalField): ValueRange = {
    field match {
      case DAY_OF_MONTH =>
        ValueRange.of(1, accessor.getMonth.minLength(), accessor.getMonth.maxLength())

      case _ =>
        super.expectedRangeFor(accessor, field)
    }
  }

  val tests = temporalAccessorTests
}

object MonthDayTest extends TestSuite {
  import ChronoField._
  import MonthDayTestUtils._

  val samples = Month.values().map(month => (month, month.minLength(), month.maxLength())).flatMap {
    case (month, minDay, maxDay) =>
      if (minDay != maxDay) Seq(MonthDay.of(month, 1), MonthDay.of(month, minDay), MonthDay.of(month, maxDay))
      else Seq(MonthDay.of(month, 1), MonthDay.of(month, minDay))
  }.toSeq

  val tests = Tests {
    'getLong - {
      assert(1L == min.getLong(MONTH_OF_YEAR))
      assert(1L == min.getLong(DAY_OF_MONTH))

      assert(12L == max.getLong(MONTH_OF_YEAR))
      assert(31L == max.getLong(DAY_OF_MONTH))
    }

    'getMonthValue - {
      assert(1 == min.getMonthValue)
      assert(12 == max.getMonthValue)
    }

    'getDayOfMonth - {
      assert(1 == min.getDayOfMonth)
      assert(31 == max.getDayOfMonth)
    }

    'getMonth - {
      assert(Month.JANUARY == min.getMonth)
      assert(Month.DECEMBER == max.getMonth)
    }

    'isValidYear - {
      for {
        t <- samples
        y <- yearSamples
      } {
        if (t == leapMonth) {
          assert(leapYear(y) == t.isValidYear(y))
        } else {
          assert(t.isValidYear(y))
        }
      }
    }

    'with - {
      assert(min == min.`with`(Month.JANUARY))
      assert(MonthDay.of(Month.FEBRUARY, 1) == min.`with`(Month.FEBRUARY))
      assert(max == max.`with`(Month.DECEMBER))
      assert(MonthDay.of(Month.NOVEMBER, 30) == max.`with`(Month.NOVEMBER))
      assert(MonthDay.of(Month.FEBRUARY, 29) == max.`with`(Month.FEBRUARY))
    }

    'withMonth - {
      assert(min == min.withMonth(1))
      assert(MonthDay.of(Month.FEBRUARY, 1) == min.withMonth(2))
      assert(max == max.withMonth(12))
      assert(MonthDay.of(Month.NOVEMBER, 30) == max.withMonth(11))
      assert(MonthDay.of(Month.FEBRUARY, 29) == max.withMonth(2))

      for (t <- samples) {
        intercept[DateTimeException](t.withMonth(Int.MinValue))
        intercept[DateTimeException](t.withMonth(Int.MaxValue))
        intercept[DateTimeException](t.withMonth(0))
      }
    }

    'withDayOfMonth - {
      assert(min == min.withDayOfMonth(1))
      assert(MonthDay.of(Month.JANUARY, 31) == min.withDayOfMonth(31))

      assert(max == max.withDayOfMonth(31))
      assert(MonthDay.of(Month.DECEMBER, 1) == max.withDayOfMonth(1))

      for (t <- samples) {
        intercept[DateTimeException](t.withDayOfMonth(Int.MinValue))
        intercept[DateTimeException](t.withDayOfMonth(Int.MaxValue))
        intercept[DateTimeException](t.withDayOfMonth(t.getMonth.maxLength() + 1))
      }
    }

    'adjustInto - {
      // Intentionally using a leap year here to be able to test the full sample
      val leapYearDate = LocalDate.of(2016, 1, 1)
      for (t <- samples) {
        val expectedDate = LocalDate.of(leapYearDate.getYear,
            t.getMonthValue, t.getDayOfMonth)
        assert(expectedDate == t.adjustInto(leapYearDate))
      }

      val nonLeapYearDate = LocalDate.of(2015, 1, 1)
      assert(LocalDate.of(2015, 2, 28) == leapMonth.adjustInto(nonLeapYearDate))
    }

    'atYear - {
      val years = Seq(-999999999, 0, 1, 999999999)
      for {
        y <- years
        t <- samples if !(t.getMonthValue == 2 && t.getDayOfMonth == 29)
      } {
        assert(LocalDate.of(y, t.getMonthValue, t.getDayOfMonth) == t.atYear(y))
      }

      val invalidYears = Seq(Int.MinValue, -1000000000, 1000000000, Int.MaxValue)
      for (t <- samples; y <- invalidYears) {
        intercept[DateTimeException](t.atYear(y))
      }

      for (y <- yearSamples) {
        val expectedDay = if (leapYear(y)) 29 else 28
        assert(LocalDate.of(y, 2, expectedDay) == leapMonth.atYear(y))
      }
    }

    'compareTo - {
      assert(min.compareTo(min) == 0)
      assert(min.compareTo(max) < 0)
      assert(max.compareTo(min) > 0)
      assert(max.compareTo(max) == 0)
    }

    'isAfter - {
      assert(min.isAfter(min) == false)
      assert(min.isAfter(max) == false)
      assert(max.isAfter(min))
      assert(max.isAfter(max) == false)
    }

    'isBefore - {
      assert(min.isBefore(min) == false)
      assert(min.isBefore(max))
      assert(max.isBefore(min) == false)
      assert(max.isBefore(max) == false)
    }

    'toStringOutput - {
      for (t <- samples) {
        val expected = f"--${t.getMonthValue}%02d-${t.getDayOfMonth}%02d"
        assert(expected == t.toString)
      }
    }

    // TODO: fix it in ScalaNative
    // 'now - {
    //   val now = LocalDate.now()
    //   val monthDay = MonthDay.now()
    //   assert(now.getMonthValue == monthDay.getMonthValue)
    //   assert(now.getDayOfMonth == monthDay.getDayOfMonth)
    // }

    'ofMonth - {
      intercept[NullPointerException](MonthDay.of(null, 1))
      intercept[DateTimeException](MonthDay.of(Month.JANUARY, Int.MinValue))
      intercept[DateTimeException](MonthDay.of(Month.JANUARY, Int.MaxValue))
      intercept[DateTimeException](MonthDay.of(Month.JANUARY, 32))
      intercept[DateTimeException](MonthDay.of(Month.FEBRUARY, 30))

      assert(min == MonthDay.of(Month.JANUARY, 1))
      assert(max == MonthDay.of(Month.DECEMBER, 31))
    }

    'of - {
      intercept[DateTimeException](MonthDay.of(Int.MinValue, 1))
      intercept[DateTimeException](MonthDay.of(Int.MaxValue, 1))

      assert(min == MonthDay.of(1, 1))
      assert(max == MonthDay.of(12, 31))
    }

    // TODO: fix it in Scala Native
    // 'from - {
    //   assert(min == MonthDay.from(min))
    //   assert(max == MonthDay.from(max))

    //   val now = LocalDate.now()
    //   assert(MonthDay.of(now.getMonthValue, now.getDayOfMonth) == MonthDay.from(now))
    // }
  }
}
