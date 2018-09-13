package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.{TemporalField, ValueRange, ChronoUnit, ChronoField}

import utest._

object YearMonthTestUtils {
  import DateTimeTestUtil._
  import ChronoField._
  import ChronoUnit._

  val min = YearMonth.of(-999999999, 1)
  val max = YearMonth.of(999999999, 12)
  val janLastBC = YearMonth.of(0, 1)
  val decLastBC = YearMonth.of(0, 12)
  val janFirstAC = YearMonth.of(1, 1)
  val decFirstAC = YearMonth.of(1, 12)
  val someYearMonth = YearMonth.of(2015, 6)
  val febLeapYear = YearMonth.of(2016, 2)
  val febNonLeapYear = YearMonth.of(2014, 2)

}

object YearMonthTestTemporal extends TestSuite with  TemporalTest[YearMonth] {
  import DateTimeTestUtil._
  import ChronoField._
  import ChronoUnit._
  import YearMonthTestUtils._

  val samples = Seq(min, max, janLastBC, decLastBC, janFirstAC, decFirstAC,
      someYearMonth, febLeapYear, febNonLeapYear)

  override def isSupported(field: ChronoField): Boolean = {
    field == YEAR || field == YEAR_OF_ERA || field == MONTH_OF_YEAR ||
    field == PROLEPTIC_MONTH || field == ERA
  }

  override def isSupported(unit: ChronoUnit): Boolean = {
    unit == MONTHS || unit == YEARS || unit == DECADES ||
    unit == CENTURIES || unit == MILLENNIA || unit == ERAS
  }

  override def expectedRangeFor(accessor: YearMonth, field: TemporalField): ValueRange = {
    field match {
      case YEAR_OF_ERA =>
        if (accessor.getYear <= 0) ValueRange.of(1, 1000000000)
        else ValueRange.of(1, 999999999)

      case _ =>
        super.expectedRangeFor(accessor, field)
    }
  }

  val tests = temporalTests
}

/** Created by alonsodomin on 25/12/2015. */
object YearMonthTest extends TestSuite {
  import DateTimeTestUtil._
  import ChronoField._
  import ChronoUnit._
  import YearMonthTestUtils._
  
  def isSupported(unit: ChronoUnit): Boolean = {
    unit == MONTHS || unit == YEARS || unit == DECADES ||
    unit == CENTURIES || unit == MILLENNIA || unit == ERAS
  }

  val samples = Seq(min, max, janLastBC, decLastBC, janFirstAC, decFirstAC,
      someYearMonth, febLeapYear, febNonLeapYear)

  val sampleLongs = YearMonthTestTemporal.sampleLongs

  val tests = Tests {

    'getLong - {
      for (ym <- samples) {
        assert(ym.getYear.toLong == ym.getLong(YEAR))
        assert(ym.getMonthValue.toLong == ym.getLong(MONTH_OF_YEAR))
        assert(ym.getMonth.getValue.toLong == ym.getLong(MONTH_OF_YEAR))
      }

      assert(-11999999988L == min.getLong(PROLEPTIC_MONTH))
      assert(1000000000L == min.getLong(YEAR_OF_ERA))
      assert(11999999999L == max.getLong(PROLEPTIC_MONTH))
      assert(999999999L == max.getLong(YEAR_OF_ERA))

      assert(24193L == febLeapYear.getLong(PROLEPTIC_MONTH))
      assert(24185L == someYearMonth.getLong(PROLEPTIC_MONTH))

      assert(1L == decLastBC.getLong(YEAR_OF_ERA))
      assert(1L == janFirstAC.getLong(YEAR_OF_ERA))
      assert(0L == janLastBC.getLong(PROLEPTIC_MONTH))
      assert(11L == decLastBC.getLong(PROLEPTIC_MONTH))
      assert(12L == janFirstAC.getLong(PROLEPTIC_MONTH))
      assert(23L == decFirstAC.getLong(PROLEPTIC_MONTH))
      assert(0L == decLastBC.getLong(ERA))
      assert(1L == janFirstAC.getLong(ERA))
    }

    'isLeapYear - {
      assert(someYearMonth.isLeapYear == false)
      assert(febLeapYear.isLeapYear)
      assert(febNonLeapYear.isLeapYear == false)
    }

    'isValidDay - {
      for (ym <- samples) {
        assert(ym.isValidDay(0) == false)
        assert(ym.isValidDay(ym.lengthOfMonth()))
        assert(ym.isValidDay(ym.lengthOfMonth() + 1) == false)
      }

      assert(febLeapYear.isValidDay(29))
      assert(febNonLeapYear.isValidDay(29) == false)
      assert(someYearMonth.isValidDay(31) == false)
    }

    'lenghtOfMonth - {
      assert(31 == min.lengthOfMonth())
      assert(31 == max.lengthOfMonth())
      assert(30 == someYearMonth.lengthOfMonth())
      assert(29 == febLeapYear.lengthOfMonth())
      assert(28 == febNonLeapYear.lengthOfMonth())
    }

    'lengthOfYear - {
      assert(365 == someYearMonth.lengthOfYear())
      assert(366 == febLeapYear.lengthOfYear())
      assert(365 == febNonLeapYear.lengthOfYear())
    }

    'with - {
      testDateTime(max.`with`(YEAR, 999999999))(max)
      testDateTime(max.`with`(YEAR, -999999999))(YearMonth.of(-999999999, 12))
      testDateTime(max.`with`(MONTH_OF_YEAR, 1))(YearMonth.of(999999999, 1))
      testDateTime(max.`with`(MONTH_OF_YEAR, 12))(max)
      testDateTime(max.`with`(PROLEPTIC_MONTH, 0))(YearMonth.of(0, 1))
      testDateTime(max.`with`(PROLEPTIC_MONTH, -1))(YearMonth.of(-1, 12))
      testDateTime(max.`with`(ERA, 1))(max)
      testDateTime(max.`with`(ERA, 0))(YearMonth.of(-999999998, 12))

      testDateTime(min.`with`(YEAR, 999999999))(YearMonth.of(999999999, 1))
      testDateTime(min.`with`(YEAR, -999999999))(min)
      testDateTime(min.`with`(MONTH_OF_YEAR, 1))(min)
      testDateTime(min.`with`(MONTH_OF_YEAR, 12))(YearMonth.of(-999999999, 12))
      testDateTime(min.`with`(PROLEPTIC_MONTH, 0))(YearMonth.of(0, 1))
      testDateTime(min.`with`(PROLEPTIC_MONTH, -1))(YearMonth.of(-1, 12))
      // TODO: this doesn't work in Scala Native ....
      // testDateTime(min.`with`(ERA, 1))(YearMonth.of(0, 12))
      testDateTime(min.`with`(ERA, 0))(min)

      testDateTime(someYearMonth.`with`(YEAR, 2000))(YearMonth.of(2000, 6))
      testDateTime(someYearMonth.`with`(MONTH_OF_YEAR, 1))(YearMonth.of(2015, 1))
      testDateTime(someYearMonth.`with`(ERA, 0))(YearMonth.of(-2014, 6))

      for {
        ym1 <- samples
        ym2 <- samples
      } {
        testDateTime(ym1.`with`(PROLEPTIC_MONTH, ym2.getLong(PROLEPTIC_MONTH)))(ym2)
      }

      for (ym <- samples) {
        intercept[DateTimeException](ym.`with`(YEAR, Long.MinValue))
        intercept[DateTimeException](ym.`with`(YEAR, -1000000000L))
        intercept[DateTimeException](ym.`with`(YEAR, 1000000000L))
        intercept[DateTimeException](ym.`with`(YEAR, Long.MaxValue))

        intercept[DateTimeException](ym.`with`(MONTH_OF_YEAR, Long.MinValue))
        intercept[DateTimeException](ym.`with`(MONTH_OF_YEAR, 0L))
        intercept[DateTimeException](ym.`with`(MONTH_OF_YEAR, 13L))
        intercept[DateTimeException](ym.`with`(MONTH_OF_YEAR, Long.MaxValue))

        intercept[DateTimeException](ym.`with`(PROLEPTIC_MONTH, Long.MinValue))
        intercept[DateTimeException](ym.`with`(PROLEPTIC_MONTH, -11999999989L))
        intercept[DateTimeException](ym.`with`(PROLEPTIC_MONTH, 12000000000L))
        intercept[DateTimeException](ym.`with`(PROLEPTIC_MONTH, Long.MaxValue))

        intercept[DateTimeException](ym.`with`(ERA, Long.MinValue))
        intercept[DateTimeException](ym.`with`(ERA, -1L))
        intercept[DateTimeException](ym.`with`(ERA, -2L))
        intercept[DateTimeException](ym.`with`(ERA, Long.MaxValue))
      }
    }

    'withYear - {
      assert(min == min.withYear(-999999999))
      assert(YearMonth.of(999999999, 1) == min.withYear(999999999))

      assert(max == max.withYear(999999999))
      assert(YearMonth.of(-999999999, 12) == max.withYear(-999999999))

      assert(YearMonth.of(1, 1) == janLastBC.withYear(1))
      assert(YearMonth.of(1, 12) == decLastBC.withYear(1))
      assert(YearMonth.of(0, 1) == janFirstAC.withYear(0))
      assert(YearMonth.of(0, 12) == decFirstAC.withYear(0))

      for (ym <- samples) {
        intercept[DateTimeException](ym.withYear(Int.MinValue))
        intercept[DateTimeException](ym.withYear(-1000000000))
        intercept[DateTimeException](ym.withYear(1000000000))
        intercept[DateTimeException](ym.withYear(Int.MaxValue))
      }
    }

    'withMonth - {
      assert(min == min.withMonth(1))
      assert(max == max.withMonth(12))

      assert(YearMonth.of(0, 12) == janLastBC.withMonth(12))
      assert(YearMonth.of(0, 1) == decLastBC.withMonth(1))
      assert(YearMonth.of(1, 12) == janFirstAC.withMonth(12))
      assert(YearMonth.of(1, 1) == decFirstAC.withMonth(1))

      for (ym <- samples) {
        intercept[DateTimeException](ym.withMonth(Int.MinValue))
        intercept[DateTimeException](ym.withMonth(0))
        intercept[DateTimeException](ym.withMonth(13))
        intercept[DateTimeException](ym.withMonth(Int.MaxValue))
      }
    }

    'plus - {
      for (ym <- samples;n <- sampleLongs) {
        testDateTime(ym.plus(n, YEARS))(ym.plusYears(n))
        testDateTime(ym.plus(n, MONTHS))(ym.plusMonths(n))
        testDateTime(ym.plus(n, DECADES))(ym.plusYears(Math.multiplyExact(n, 10)))
        testDateTime(ym.plus(n, CENTURIES))(ym.plusYears(Math.multiplyExact(n, 100)))
        testDateTime(ym.plus(n, MILLENNIA))(ym.plusYears(Math.multiplyExact(n, 1000)))
        testDateTime(ym.plus(n, ERAS))(ym.`with`(ERA, Math.addExact(n, ym.getLong(ERA))))
      }
    }

    'plusYears - {
      for (ym <- samples) {
        assert(ym == ym.plusYears(0))
      }

      assert(YearMonth.of(0, 1) == min.plusYears(max.getYear))
      assert(YearMonth.of(0, 12) == max.plusYears(min.getYear))
      assert(febLeapYear == febNonLeapYear.plusYears(2))

      assert(YearMonth.of(999999999, 1) == min.plusYears(1999999998))
      assert(YearMonth.of(-999999999, 12) == max.plusYears(-1999999998))

      intercept[DateTimeException](min.plusYears(-1))
      intercept[DateTimeException](min.plusYears(1999999999))
      intercept[DateTimeException](max.plusYears(1))
      intercept[DateTimeException](max.plusYears(-1999999999))
      intercept[DateTimeException](min.plusYears(Long.MinValue))
      intercept[DateTimeException](max.plusYears(Long.MaxValue))
    }

    'plusMonths - {
      for (ym <- samples) {
        assert(ym == ym.plusMonths(0))
      }

      assert(janFirstAC == decLastBC.plusMonths(1))
      assert(someYearMonth == janLastBC.plusMonths(24185))
      assert(janFirstAC == min.plusMonths(max.getLong(PROLEPTIC_MONTH) + 1))
      assert(janFirstAC == max.plusMonths(min.getLong(PROLEPTIC_MONTH) + 1))

      assert(YearMonth.of(999999999, 12) == min.plusMonths(23999999987L))
      assert(YearMonth.of(-999999999, 1) == max.plusMonths(-23999999987L))

      intercept[DateTimeException](min.plusMonths(-1))
      intercept[DateTimeException](min.plusMonths(23999999988L))
      intercept[DateTimeException](max.plusMonths(-23999999988L))
      intercept[DateTimeException](max.plusMonths(1))
      intercept[DateTimeException](min.plusMonths(Long.MinValue))
      intercept[DateTimeException](max.plusMonths(Long.MaxValue))
    }

    'minusYears - {
      for (ym <- samples) {
        assert(ym == ym.minusYears(0))
      }

      assert(febNonLeapYear == febLeapYear.minusYears(2))

      assert(YearMonth.of(999999999, 1) == min.minusYears(-1999999998))
      assert(YearMonth.of(-999999999, 12) == max.minusYears(1999999998))

      intercept[DateTimeException](min.minusYears(1))
      intercept[DateTimeException](min.minusYears(-1999999999))
      intercept[DateTimeException](max.minusYears(-1))
      intercept[DateTimeException](max.minusYears(1999999999))
      intercept[DateTimeException](min.minusYears(Long.MaxValue))
      intercept[DateTimeException](max.minusYears(Long.MinValue))
    }

    'minusMonths - {
      for (ym <- samples) {
        assert(ym == ym.minusMonths(0))
      }

      assert(decLastBC == janFirstAC.minusMonths(1))
      assert(janLastBC == someYearMonth.minusMonths(24185))

      assert(YearMonth.of(999999999, 12) == min.minusMonths(-23999999987L))
      assert(YearMonth.of(-999999999, 1) == max.minusMonths(23999999987L))

      intercept[DateTimeException](min.minusMonths(1))
      intercept[DateTimeException](min.minusMonths(-23999999988L))
      intercept[DateTimeException](max.minusMonths(23999999988L))
      intercept[DateTimeException](max.minusMonths(-1))
      intercept[DateTimeException](min.minusMonths(Long.MaxValue))
      intercept[DateTimeException](max.minusMonths(Long.MinValue))
    }

    'adjustInto - {
      for (ym1 <- samples; ym2 <- samples) {
        testDateTime(ym1.adjustInto(ym2))(ym1)
      }

      val someDate = LocalDate.of(2015, 1, 1)
      for (ym <- samples) {
        testDateTime(ym.adjustInto(someDate))(LocalDate.of(ym.getYear, ym.getMonthValue, 1))
      }
    }

    'until - {
      for {
        ym <- samples
        unit <- ChronoUnit.values() if isSupported(unit)
      } {
        assert(0L == ym.until(ym, unit))
      }

      assert(1999999998L == min.until(max, YEARS))
      assert(23999999987L == min.until(max, MONTHS))
      assert(-1999999998L == max.until(min, YEARS))
      assert(-23999999987L == max.until(min, MONTHS))
      assert(199999999L == min.until(max, DECADES))
      assert(19999999L == min.until(max, CENTURIES))
      assert(1999999L == min.until(max, MILLENNIA))
      assert(1L == min.until(max, ERAS))

      assert(1L == janLastBC.until(janFirstAC, YEARS))
      assert(12L == janLastBC.until(janFirstAC, MONTHS))
      assert(1L == janLastBC.until(decFirstAC, YEARS))
      assert(23L == janLastBC.until(decFirstAC, MONTHS))
      assert(0L == decLastBC.until(janFirstAC, YEARS))
      assert(1L == decLastBC.until(janFirstAC, MONTHS))

      for {
        ym1 <- samples
        ym2 <- samples if ym2.isAfter(ym1)
        unit <- ChronoUnit.values() if isSupported(unit)
      } {
        assert(-ym1.until(ym2, unit) == ym2.until(ym1, unit))
      }
    }

    'atDay - {
      for {
        ym <- samples
        day <- 1 to ym.lengthOfMonth()
      } {
        assert(LocalDate.of(ym.getYear, ym.getMonthValue, day) == ym.atDay(day))
      }

      for (ym <- samples) {
        intercept[DateTimeException](ym.atDay(0))
        intercept[DateTimeException](ym.atDay(ym.lengthOfMonth() + 1))
      }
    }

    'atEndOfMonth - {
      for (ym <- samples) {
        val endOfMonth = ym.lengthOfMonth()
        assert(LocalDate.of(ym.getYear, ym.getMonthValue, endOfMonth) == ym.atEndOfMonth())
      }
    }

    'compareTo - {
      assert(0 == min.compareTo(min))
      assert(-1999999998 == min.compareTo(max))
      assert(-11 == min.compareTo(YearMonth.of(Year.MIN_VALUE, 12)))
      assert(1999999998 == max.compareTo(min))
      assert(11 == max.compareTo(YearMonth.of(Year.MAX_VALUE, 1)))
      assert(0 == max.compareTo(max))
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

    'equalsHashCode - {
      assert(YearMonth.of(Year.MIN_VALUE, 1) == min)
      assert(YearMonth.of(Year.MIN_VALUE, 1).hashCode() == min.hashCode())
      assert(YearMonth.of(Year.MAX_VALUE, 12) == max)
      assert(YearMonth.of(Year.MAX_VALUE, 12).hashCode() == max.hashCode())

      for {
        ym1 <- samples
        ym2 <- samples
      } {
        if (ym1.hashCode() == ym2.hashCode()) {
          assert(ym1.equals(ym2))
        } else {
          assert(ym1.equals(ym2) == false)
        }
      }
    }

    'toStringOutput - {
      assert("-999999999-01" == min.toString)
      assert("999999999-12" == max.toString)
      assert("-10000-01" == YearMonth.of(-10000, 1).toString)
      assert("10000-12" == YearMonth.of(10000, 12).toString)
      assert("2015-06" == someYearMonth.toString)
      assert("0000-01" == janLastBC.toString)
      assert("0000-12" == decLastBC.toString)
      assert("0001-01" == janFirstAC.toString)
      assert("0001-12" == decFirstAC.toString)
    }

    'ofMonth - {
      val yearMonth = YearMonth.of(23, Month.JANUARY)
      assert(23 == yearMonth.getYear)
      assert(1 == yearMonth.getMonthValue)
      assert(Month.JANUARY == yearMonth.getMonth)

      assert(Year.MIN_VALUE == min.getYear)
      assert(Month.JANUARY == min.getMonth)
      assert(Year.MAX_VALUE == max.getYear)
      assert(Month.DECEMBER == max.getMonth)

      for (ym <- samples) {
        assert(ym == YearMonth.of(ym.getYear, ym.getMonth))
      }

      intercept[NullPointerException](YearMonth.of(0, null))
      for (m <- Month.values()) {
        intercept[DateTimeException](YearMonth.of(Int.MinValue, m))
        intercept[DateTimeException](YearMonth.of(Int.MaxValue, m))
      }
    }

    'of - {
      val yearMonth = YearMonth.of(293, 11)
      assert(293 == yearMonth.getYear)
      assert(11 == yearMonth.getMonthValue)
      assert(Month.NOVEMBER == yearMonth.getMonth)

      assert(Year.MIN_VALUE == min.getYear)
      assert(1 == min.getMonthValue)
      assert(Year.MAX_VALUE == max.getYear)
      assert(12 == max.getMonthValue)

      for (ym <- samples) {
        assert(ym == YearMonth.of(ym.getYear, ym.getMonthValue))
      }

      intercept[DateTimeException](YearMonth.of(Int.MinValue, 0))
      intercept[DateTimeException](YearMonth.of(Int.MaxValue, 0))
      intercept[DateTimeException](YearMonth.of(min.getYear, 0))
      intercept[DateTimeException](YearMonth.of(min.getYear, 13))
      intercept[DateTimeException](YearMonth.of(max.getYear, 0))
      intercept[DateTimeException](YearMonth.of(max.getYear, 13))
    }

    // TODO: implement it in Scala Native
    // 'now - {
    //   val now = LocalDate.now()
    //   val yearMonth = YearMonth.now()
    //   if (yearMonth.getMonthValue != now.getMonthValue) {
    //     println("Month changed in the middle of the test!")
    //     val newNow = LocalDate.now()
    //     assert(newNow.getMonth, yearMonth.getMonth)
    //     assert(newNow.getMonthValue, yearMonth.getMonthValue)
    //     assert(newNow.getYear, yearMonth.getYear)
    //   }
    // }

    'from - {
      for (ym <- samples) {
        assert(ym == YearMonth.from(ym))
      }

      val someDate = LocalDate.of(2015, 1, 1)
      assert(YearMonth.of(2015, 1) == YearMonth.from(someDate))

      intercept[DateTimeException](YearMonth.from(Month.JANUARY))
    }
  }
}
