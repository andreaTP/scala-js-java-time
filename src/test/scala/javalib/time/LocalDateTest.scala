package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.chrono.{IsoEra, IsoChronology}
import java.time.format.DateTimeParseException
import java.time.temporal._

import utest._

object LocalDateUtils {
  import DateTimeTestUtil._
  import LocalDate._
  import ChronoField._
  import ChronoUnit._

  val someDate = of(2011, 2, 28)
  val leapDate = of(2012, 2, 29)
}

object LocalDateTestTemporal extends TestSuite with TemporalTest[LocalDate] {
  import DateTimeTestUtil._
  import LocalDate._
  import ChronoField._
  import ChronoUnit._
  import LocalDateUtils._

  val samples = Seq(MIN, ofEpochDay(-1), ofEpochDay(0), ofEpochDay(1), someDate, leapDate, MAX)

  def isSupported(unit: ChronoUnit): Boolean = unit.isDateBased

  def isSupported(field: ChronoField): Boolean = field.isDateBased

  val tests = temporalTests

}

object LocalDateTest extends TestSuite {
  import DateTimeTestUtil._
  import LocalDate._
  import ChronoField._
  import ChronoUnit._
  import LocalDateUtils._

  val samples = Seq(MIN, ofEpochDay(-1), ofEpochDay(0), ofEpochDay(1), someDate,
      leapDate, MAX)

  val tests = Tests {

    'test_getLong - {
      for (d <- samples) {
        assert(d.getDayOfWeek.getValue.toLong == d.getLong(DAY_OF_WEEK))
        assert(d.getDayOfMonth.toLong == d.getLong(DAY_OF_MONTH))
        assert(d.getDayOfYear.toLong == d.getLong(DAY_OF_YEAR))
        assert(d.toEpochDay == d.getLong(EPOCH_DAY))
        assert(d.getMonthValue.toLong == d.getLong(MONTH_OF_YEAR))
        assert(d.getYear.toLong == d.getLong(YEAR))
        assert(d.getEra.getValue.toLong == d.getLong(ERA))
      }

      assert(1L == MIN.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
      assert(1L == MIN.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
      assert(1L == MIN.getLong(ALIGNED_WEEK_OF_MONTH))
      assert(1L == MIN.getLong(ALIGNED_WEEK_OF_YEAR))
      assert(-11999999988L == MIN.getLong(PROLEPTIC_MONTH))
      assert(1000000000L == MIN.getLong(YEAR_OF_ERA))

      assert(7L == someDate.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
      assert(3L == someDate.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
      assert(4L == someDate.getLong(ALIGNED_WEEK_OF_MONTH))
      assert(9L == someDate.getLong(ALIGNED_WEEK_OF_YEAR))
      assert(24133L == someDate.getLong(PROLEPTIC_MONTH))
      assert(2011L == someDate.getLong(YEAR_OF_ERA))

      assert(1L == leapDate.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
      assert(4L == leapDate.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
      assert(5L == leapDate.getLong(ALIGNED_WEEK_OF_MONTH))
      assert(9L == leapDate.getLong(ALIGNED_WEEK_OF_YEAR))
      assert(24145L == leapDate.getLong(PROLEPTIC_MONTH))
      assert(2012L == leapDate.getLong(YEAR_OF_ERA))

      assert(3L == MAX.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
      assert(1L == MAX.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
      assert(5L == MAX.getLong(ALIGNED_WEEK_OF_MONTH))
      assert(53L == MAX.getLong(ALIGNED_WEEK_OF_YEAR))
      assert(11999999999L == MAX.getLong(PROLEPTIC_MONTH))
      assert(999999999L == MAX.getLong(YEAR_OF_ERA))

    }

    'test_getChronology - {
      for (d <- samples)
        assert(IsoChronology.INSTANCE == d.getChronology)
    }

    'test_getEra - {
      assert(IsoEra.BCE == MIN.getEra)
      assert(IsoEra.CE == someDate.getEra)
      assert(IsoEra.CE == leapDate.getEra)
      assert(IsoEra.CE == MAX.getEra)
    }

    'test_getYear - {
      assert(-999999999 == MIN.getYear)
      assert(2011 == someDate.getYear)
      assert(2012 == leapDate.getYear)
      assert(999999999 == MAX.getYear)
    }

    'test_getMonthValue - {
      for (d <- samples)
        assert(d.getMonth.getValue == d.getMonthValue)
    }

    'test_getMonth - {
      assert(Month.JANUARY == MIN.getMonth)
      assert(Month.FEBRUARY == someDate.getMonth)
      assert(Month.FEBRUARY == leapDate.getMonth)
      assert(Month.DECEMBER == MAX.getMonth)
    }

    'test_getDayOfMonth - {
      assert(1 == MIN.getDayOfMonth)
      assert(28 == someDate.getDayOfMonth)
      assert(29 == leapDate.getDayOfMonth)
      assert(31 == MAX.getDayOfMonth)
    }

    'test_getDayOfYear - {
      assert(1 == MIN.getDayOfYear)
      assert(59 == someDate.getDayOfYear)
      assert(60 == leapDate.getDayOfYear)
      assert(366 == of(2012, 12, 31).getDayOfYear)
      assert(365 == MAX.getDayOfYear)
    }

    'test_getDayOfWeek - {
      assert(DayOfWeek.MONDAY == MIN.getDayOfWeek)
      assert(DayOfWeek.MONDAY == someDate.getDayOfWeek)
      assert(DayOfWeek.WEDNESDAY == leapDate.getDayOfWeek)
      assert(DayOfWeek.FRIDAY == MAX.getDayOfWeek)
    }

    'test_isLeapYear - {
      assert(MIN.isLeapYear == false)
      assert(of(-400, 6, 30).isLeapYear)
      assert(of(-100, 3, 1).isLeapYear == false)
      assert(of(0, 1, 1).isLeapYear)
      assert(of(1900, 9, 30).isLeapYear == false)
      assert(of(2000, 1, 1).isLeapYear)
      assert(someDate.isLeapYear == false)
      assert(leapDate.isLeapYear)
      assert(MAX.isLeapYear == false)
    }

    'test_lengthOfMonth - {
      for (d <- samples ++ Seq(of(2001, 2, 1), of(2012, 9, 30)))
        assert(d.getMonth.length(d.isLeapYear) == d.lengthOfMonth)
    }

    'test_lengthOfYear - {
      for (d <- samples)
        assert({if (d.isLeapYear) 366 else 365} == d.lengthOfYear)
    }

    'test_with - {
      testDateTime(MAX.`with`(DAY_OF_WEEK, 1))(of(999999999, 12, 27))
      testDateTime(MAX.`with`(DAY_OF_WEEK, 5))(MAX)
      testDateTime(MIN.`with`(DAY_OF_WEEK, 1))(MIN)
      testDateTime(MIN.`with`(DAY_OF_WEEK, 7))(of(-999999999, 1, 7))
      testDateTime(someDate.`with`(DAY_OF_WEEK, 1))(someDate)
      testDateTime(someDate.`with`(DAY_OF_WEEK, 7))(of(2011, 3, 6))
      testDateTime(leapDate.`with`(DAY_OF_WEEK, 1))(of(2012, 2, 27))
      testDateTime(leapDate.`with`(DAY_OF_WEEK, 7))(of(2012, 3, 4))
      testDateTime(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(of(999999999, 12, 29))
      testDateTime(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 3))(MAX)
      testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(MIN)
      testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(of(-999999999, 1, 7))
      testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(of(2011, 2, 22))
      testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(someDate)
      testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(leapDate)
      testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(of(2012, 3, 6))
      testDateTime(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(MAX)
      testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(MIN)
      testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(-999999999, 1, 7))
      testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(of(2011, 2, 26))
      testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(2011, 3, 4))
      testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(of(2012, 2, 26))
      testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(2012, 3, 3))
      testDateTime(someDate.`with`(DAY_OF_MONTH, 1))(of(2011, 2, 1))
      testDateTime(leapDate.`with`(DAY_OF_MONTH, 28))(of(2012, 2, 28))
      testDateTime(someDate.`with`(DAY_OF_YEAR, 1))(of(2011, 1, 1))
      testDateTime(someDate.`with`(DAY_OF_YEAR, 365))(of(2011, 12, 31))
      testDateTime(leapDate.`with`(DAY_OF_YEAR, 366))(of(2012, 12, 31))
      for {
        d1 <- samples
        d2 <- samples
      } {
        testDateTime(d1.`with`(EPOCH_DAY, d2.toEpochDay))(d2)
      }
      testDateTime(MAX.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(999999999, 12, 3))
      testDateTime(MAX.`with`(ALIGNED_WEEK_OF_MONTH, 5))(MAX)
      testDateTime(MIN.`with`(ALIGNED_WEEK_OF_MONTH, 1))(MIN)
      testDateTime(MIN.`with`(ALIGNED_WEEK_OF_MONTH, 5))(of(-999999999, 1, 29))
      testDateTime(someDate.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(2011, 2, 7))
      testDateTime(someDate.`with`(ALIGNED_WEEK_OF_MONTH, 5))(of(2011, 3, 7))
      testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(2012, 2, 1))
      testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_MONTH, 5))(leapDate)
      testDateTime(MAX.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(999999999, 1, 1))
      testDateTime(MAX.`with`(ALIGNED_WEEK_OF_YEAR, 53))(MAX)
      testDateTime(MIN.`with`(ALIGNED_WEEK_OF_YEAR, 1))(MIN)
      testDateTime(MIN.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(-999999999, 12, 31))
      testDateTime(someDate.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(2011, 1, 3))
      testDateTime(someDate.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(2012, 1, 2))
      testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(2012, 1, 4))
      testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(2013, 1, 2))
      testDateTime(MAX.`with`(MONTH_OF_YEAR, 2))(of(999999999, 2, 28))
      testDateTime(MAX.`with`(MONTH_OF_YEAR, 11))(of(999999999, 11, 30))
      testDateTime(someDate.`with`(MONTH_OF_YEAR, 1))(of(2011, 1, 28))
      testDateTime(leapDate.`with`(MONTH_OF_YEAR, 2))(leapDate)
      testDateTime(MAX.`with`(PROLEPTIC_MONTH, 1))(of(0, 2, 29))
      testDateTime(MIN.`with`(PROLEPTIC_MONTH, -1))(of(-1, 12, 1))
      testDateTime(someDate.`with`(PROLEPTIC_MONTH, -11999999988L))(of(-999999999, 1, 28))
      testDateTime(leapDate.`with`(PROLEPTIC_MONTH, 11999999999L))(of(999999999, 12, 29))
      testDateTime(MIN.`with`(YEAR_OF_ERA, 1000000000))(MIN)
      testDateTime(MIN.`with`(YEAR_OF_ERA, 1))(of(0, 1, 1))
      testDateTime(MAX.`with`(YEAR_OF_ERA, 999999999))(MAX)
      testDateTime(MAX.`with`(YEAR_OF_ERA, 1))(of(1, 12, 31))
      testDateTime(leapDate.`with`(YEAR_OF_ERA, 2011))(someDate)
      testDateTime(MIN.`with`(YEAR, -999999999))(MIN)
      testDateTime(MIN.`with`(YEAR, 999999999))(of(999999999, 1, 1))
      testDateTime(MAX.`with`(YEAR, -999999999))(of(-999999999, 12, 31))
      testDateTime(MAX.`with`(YEAR, 999999999))(MAX)
      testDateTime(leapDate.`with`(YEAR, 2011))(someDate)
      testDateTime(MIN.`with`(ERA, 0))(MIN)
      testDateTime(MAX.`with`(ERA, 0))(of(-999999998, 12, 31))
      testDateTime(MAX.`with`(ERA, 1))(MAX)
      testDateTime(someDate.`with`(ERA, 1))(someDate)
      testDateTime(leapDate.`with`(ERA, 0))(of(-2011, 2, 28))

      intercept[DateTimeException](MAX.`with`(DAY_OF_WEEK, 6))
      intercept[DateTimeException](MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 4))
      intercept[DateTimeException](MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 2))
      intercept[DateTimeException](someDate.`with`(DAY_OF_MONTH, 29))
      intercept[DateTimeException](leapDate.`with`(DAY_OF_MONTH, 30))
      intercept[DateTimeException](someDate.`with`(DAY_OF_YEAR, 366))
      intercept[DateTimeException](someDate.`with`(YEAR_OF_ERA, 1000000000))
      intercept[DateTimeException](MIN.`with`(ERA, 1))

      for (d <- samples) {
        for (n <- Seq(Long.MinValue, 0L, 8L, Long.MaxValue)) {
          intercept[DateTimeException](d.`with`(DAY_OF_WEEK, n))
          intercept[DateTimeException](d.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, n))
          intercept[DateTimeException](d.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 32L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(DAY_OF_MONTH, n))
        for (n <- Seq(Long.MinValue, 0L, 367L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(DAY_OF_YEAR, n))
        for (n <- Seq(Long.MinValue, -365243219163L, 365241780472L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(EPOCH_DAY, n))
        for (n <- Seq(Long.MinValue, 0L, 6L, Long.MaxValue)) {
          intercept[DateTimeException](d.`with`(ALIGNED_WEEK_OF_MONTH, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 54L, Long.MaxValue)) {
          intercept[DateTimeException](d.`with`(ALIGNED_WEEK_OF_YEAR, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 13L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(MONTH_OF_YEAR, n))
        for (n <- Seq(Long.MinValue, -11999999989L, 12000000000L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(PROLEPTIC_MONTH, n))
        for (n <- Seq(Long.MinValue, 0L, 1000000001L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(YEAR_OF_ERA, n))
        for (n <- Seq(Long.MinValue, -1000000000L, 1000000000L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(YEAR, n))
        for (n <- Seq(Long.MinValue, -1L, 2L, Long.MaxValue))
          intercept[DateTimeException](d.`with`(ERA, n))
      }
    }

    'test_withYear - {
      testDateTime(MIN.withYear(-999999999))(MIN)
      testDateTime(MIN.withYear(999999999))(of(999999999, 1, 1))
      testDateTime(MAX.withYear(-999999999))(of(-999999999, 12, 31))
      testDateTime(MAX.withYear(999999999))(MAX)

      val years = Seq(Int.MinValue, -1000000000, 1000000000, Int.MaxValue)
      for {
        d <- samples
        n <- years
      } {
        intercept[DateTimeException](d.withYear(n))
      }
    }

    'test_withMonth - {
      testDateTime(MAX.withMonth(2))(of(999999999, 2, 28))
      testDateTime(MAX.withMonth(11))(of(999999999, 11, 30))
      testDateTime(someDate.withMonth(1))(of(2011, 1, 28))
      testDateTime(leapDate.withMonth(2))(leapDate)

      val months = Seq(Int.MinValue, 0, 13, Int.MaxValue)
      for {
        d <- samples
        n <- months
      } {
        intercept[DateTimeException](d.withMonth(n))
      }
    }

    'test_withDayOfMonth - {
      testDateTime(someDate.withDayOfMonth(1))(of(2011, 2, 1))
      testDateTime(leapDate.withDayOfMonth(28))(of(2012, 2, 28))

      intercept[DateTimeException](someDate.withDayOfMonth(29))
      intercept[DateTimeException](leapDate.withDayOfMonth(30))
      intercept[DateTimeException](of(0, 4, 30).withDayOfMonth(31))
      val days = Seq(Int.MinValue, 0, 32, Int.MaxValue)
      for {
        d <- samples
        n <- days
      } {
        intercept[DateTimeException](d.withDayOfMonth(n))
      }
    }

    'test_withDayOfYear - {
      testDateTime(someDate.withDayOfYear(1))(of(2011, 1, 1))
      testDateTime(someDate.withDayOfYear(365))(of(2011, 12, 31))
      testDateTime(leapDate.withDayOfYear(366))(of(2012, 12, 31))

      intercept[DateTimeException](someDate.withDayOfYear(366))
      val days = Seq(Int.MinValue, 0, 367, Int.MaxValue)
      for {
        d <- samples
        n <- days
      } {
        intercept[DateTimeException](d.withDayOfMonth(n))
      }
    }

    'test_plus - {
      val values = Seq(Long.MinValue, Int.MinValue.toLong, -1000L, -366L, -365L,
          -100L, -12L, -10L, -7L, -1L, 0L, 1L, 7L, 10L, 12L, 100L,
          365L, 366L, 1000L, Int.MaxValue.toLong, Long.MaxValue)

      for {
        d <- samples
        n <- values
      } {
        testDateTime(d.plus(n, DAYS))(d.plusDays(n))
        testDateTime(d.plus(n, WEEKS))(d.plusWeeks(n))
        testDateTime(d.plus(n, MONTHS))(d.plusMonths(n))
        testDateTime(d.plus(n, YEARS))(d.plusYears(n))
        testDateTime(d.plus(n, DECADES))(d.plusYears(Math.multiplyExact(n, 10)))
        testDateTime(d.plus(n, CENTURIES))(d.plusYears(Math.multiplyExact(n, 100)))
        testDateTime(d.plus(n, MILLENNIA))(d.plusYears(Math.multiplyExact(n, 1000)))
        testDateTime(d.plus(n, ERAS))(d.`with`(ERA, Math.addExact(n, d.get(ERA))))
      }
    }

    'test_plusYears - {
      for (d <- samples)
        testDateTime(d.plusYears(0))(d)
      testDateTime(someDate.plusYears(-2))(of(2009, 2, 28))
      testDateTime(someDate.plusYears(-1))(of(2010, 2, 28))
      testDateTime(someDate.plusYears(1))(of(2012, 2, 28))
      testDateTime(someDate.plusYears(2))(of(2013, 2, 28))
      testDateTime(leapDate.plusYears(-2))(of(2010, 2, 28))
      testDateTime(leapDate.plusYears(-1))(someDate)
      testDateTime(leapDate.plusYears(1))(of(2013, 2, 28))
      testDateTime(leapDate.plusYears(2))(of(2014, 2, 28))
      testDateTime(MIN.plusYears(1999999998))(of(999999999, 1, 1))
      testDateTime(MAX.plusYears(-1999999998))(of(-999999999, 12, 31))
      intercept[DateTimeException](MIN.plusYears(-1))
      intercept[DateTimeException](MIN.plusYears(1999999999))
      intercept[DateTimeException](MAX.plusYears(-1999999999))
      intercept[DateTimeException](MAX.plusYears(1))
      intercept[DateTimeException](MIN.plusYears(Long.MinValue))
      intercept[DateTimeException](MAX.plusYears(Long.MaxValue))
    }

    'test_plusMonths - {
      for (d <- samples)
        testDateTime(d.plusMonths(0))(d)
      testDateTime(someDate.plusMonths(-12))(of(2010, 2, 28))
      testDateTime(someDate.plusMonths(-1))(of(2011, 1, 28))
      testDateTime(someDate.plusMonths(1))(of(2011, 3, 28))
      testDateTime(someDate.plusMonths(12))(of(2012, 2, 28))
      testDateTime(leapDate.plusMonths(-12))(someDate)
      testDateTime(leapDate.plusMonths(-1))(of(2012, 1, 29))
      testDateTime(leapDate.plusMonths(1))(of(2012, 3, 29))
      testDateTime(leapDate.plusMonths(12))(of(2013, 2, 28))
      testDateTime(of(2011, 1, 31).plusMonths(1))(someDate)
      testDateTime(of(2011, 3, 31).plusMonths(-1))(someDate)
      testDateTime(of(2011, 3, 31).plusMonths(1))(of(2011, 4, 30))
      testDateTime(of(2012, 1, 31).plusMonths(1))(leapDate)
      testDateTime(of(2012, 3, 31).plusMonths(-1))(leapDate)
      testDateTime(of(2012, 3, 31).plusMonths(1))(of(2012, 4, 30))
      testDateTime(MIN.plusMonths(23999999987L))(of(999999999, 12, 1))
      testDateTime(MAX.plusMonths(-23999999987L))(of(-999999999, 1, 31))
      intercept[DateTimeException](MIN.plusMonths(-1))
      intercept[DateTimeException](MIN.plusMonths(23999999988L))
      intercept[DateTimeException](MAX.plusMonths(-23999999988L))
      intercept[DateTimeException](MAX.plusMonths(1))
      intercept[DateTimeException](MIN.plusMonths(Long.MinValue))
      intercept[DateTimeException](MAX.plusMonths(Long.MaxValue))
    }

    'test_plusWeeks - {
      for (d <- samples)
        testDateTime(d.plusWeeks(0))(d)
      testDateTime(someDate.plusWeeks(-53))(of(2010, 2, 22))
      testDateTime(someDate.plusWeeks(-52))(of(2010, 3, 1))
      testDateTime(someDate.plusWeeks(-1))(of(2011, 2, 21))
      testDateTime(someDate.plusWeeks(1))(of(2011, 3, 7))
      testDateTime(someDate.plusWeeks(52))(of(2012, 2, 27))
      testDateTime(someDate.plusWeeks(53))(of(2012, 3, 5))
      testDateTime(leapDate.plusWeeks(-53))(of(2011, 2, 23))
      testDateTime(leapDate.plusWeeks(-52))(of(2011, 3, 2))
      testDateTime(leapDate.plusWeeks(-1))(of(2012, 2, 22))
      testDateTime(leapDate.plusWeeks(1))(of(2012, 3, 7))
      testDateTime(leapDate.plusWeeks(52))(of(2013, 2, 27))
      testDateTime(leapDate.plusWeeks(53))(of(2013, 3, 6))
      testDateTime(MIN.plusWeeks(104354999947L))(of(999999999, 12, 27))
      testDateTime(MAX.plusWeeks(-104354999947L))(of(-999999999, 1, 5))
      intercept[DateTimeException](MIN.plusWeeks(-1))
      intercept[DateTimeException](MIN.plusWeeks(104354999948L))
      intercept[DateTimeException](MAX.plusWeeks(-1043549999478L))
      intercept[DateTimeException](MAX.plusWeeks(1))
      intercept[ArithmeticException](MIN.plusWeeks(Long.MinValue))
      intercept[ArithmeticException](MAX.plusWeeks(Long.MaxValue))
    }

    'test_plusDays - {
      for (d <- samples)
        testDateTime(d.plusDays(0))(d)
      testDateTime(someDate.plusDays(-365))(of(2010, 2, 28))
      testDateTime(someDate.plusDays(-1))(of(2011, 2, 27))
      testDateTime(someDate.plusDays(1))(of(2011, 3, 1))
      testDateTime(someDate.plusDays(365))(of(2012, 2, 28))
      testDateTime(someDate.plusDays(366))(leapDate)
      testDateTime(leapDate.plusDays(-366))(someDate)
      testDateTime(leapDate.plusDays(-365))(of(2011, 3, 1))
      testDateTime(leapDate.plusDays(-1))(of(2012, 2, 28))
      testDateTime(leapDate.plusDays(1))(of(2012, 3, 1))
      testDateTime(leapDate.plusDays(365))(of(2013, 2, 28))
      testDateTime(leapDate.plusDays(366))(of(2013, 3, 1))
      testDateTime(MIN.plusDays(730484999633L))(MAX)
      testDateTime(MAX.plusDays(-730484999633L))(MIN)
      intercept[DateTimeException](MIN.plusDays(-1))
      intercept[DateTimeException](MIN.plusDays(730484999634L))
      intercept[DateTimeException](MAX.plusDays(-730484999634L))
      intercept[DateTimeException](MAX.plusDays(1))
      intercept[ArithmeticException](ofEpochDay(-1).plusDays(Long.MinValue))
      intercept[DateTimeException](ofEpochDay(0).plusDays(Long.MinValue))
      intercept[DateTimeException](ofEpochDay(0).plusDays(Long.MaxValue))
      intercept[ArithmeticException](ofEpochDay(1).plusDays(Long.MaxValue))
    }

    'test_minusYears - {
      for (d <- samples)
        testDateTime(d.minusYears(0))(d)
      testDateTime(someDate.minusYears(2))(of(2009, 2, 28))
      testDateTime(someDate.minusYears(1))(of(2010, 2, 28))
      testDateTime(someDate.minusYears(-1))(of(2012, 2, 28))
      testDateTime(someDate.minusYears(-2))(of(2013, 2, 28))
      testDateTime(leapDate.minusYears(2))(of(2010, 2, 28))
      testDateTime(leapDate.minusYears(1))(someDate)
      testDateTime(leapDate.minusYears(-1))(of(2013, 2, 28))
      testDateTime(leapDate.minusYears(-2))(of(2014, 2, 28))
      testDateTime(MIN.minusYears(-1999999998))(of(999999999, 1, 1))
      testDateTime(MAX.minusYears(1999999998))(of(-999999999, 12, 31))
      intercept[DateTimeException](MIN.minusYears(1))
      intercept[DateTimeException](MIN.minusYears(-1999999999))
      intercept[DateTimeException](MAX.minusYears(1999999999))
      intercept[DateTimeException](MAX.minusYears(-1))
      intercept[DateTimeException](MIN.minusYears(Long.MaxValue))
      intercept[DateTimeException](MAX.minusYears(Long.MinValue))
    }

    'test_minusMonths - {
      for (d <- samples)
        testDateTime(d.minusMonths(0))(d)
      testDateTime(someDate.minusMonths(12))(of(2010, 2, 28))
      testDateTime(someDate.minusMonths(1))(of(2011, 1, 28))
      testDateTime(someDate.minusMonths(-1))(of(2011, 3, 28))
      testDateTime(someDate.minusMonths(-12))(of(2012, 2, 28))
      testDateTime(leapDate.minusMonths(12))(someDate)
      testDateTime(leapDate.minusMonths(1))(of(2012, 1, 29))
      testDateTime(leapDate.minusMonths(-1))(of(2012, 3, 29))
      testDateTime(leapDate.minusMonths(-12))(of(2013, 2, 28))
      testDateTime(of(2011, 1, 31).minusMonths(-1))(someDate)
      testDateTime(of(2011, 3, 31).minusMonths(1))(someDate)
      testDateTime(of(2011, 3, 31).minusMonths(-1))(of(2011, 4, 30))
      testDateTime(of(2012, 1, 31).minusMonths(-1))(leapDate)
      testDateTime(of(2012, 3, 31).minusMonths(1))(leapDate)
      testDateTime(of(2012, 3, 31).minusMonths(-1))(of(2012, 4, 30))
      testDateTime(MIN.minusMonths(-23999999987L))(of(999999999, 12, 1))
      testDateTime(MAX.minusMonths(23999999987L))(of(-999999999, 1, 31))
      intercept[DateTimeException](MIN.minusMonths(1))
      intercept[DateTimeException](MIN.minusMonths(-23999999988L))
      intercept[DateTimeException](MAX.minusMonths(23999999988L))
      intercept[DateTimeException](MAX.minusMonths(-1))
      intercept[DateTimeException](MIN.minusMonths(Long.MaxValue))
      intercept[DateTimeException](MAX.minusMonths(Long.MinValue))
    }

    'test_minusWeeks - {
      for (d <- samples)
        testDateTime(d.minusWeeks(0))(d)
      testDateTime(someDate.minusWeeks(53))(of(2010, 2, 22))
      testDateTime(someDate.minusWeeks(52))(of(2010, 3, 1))
      testDateTime(someDate.minusWeeks(1))(of(2011, 2, 21))
      testDateTime(someDate.minusWeeks(-1))(of(2011, 3, 7))
      testDateTime(someDate.minusWeeks(-52))(of(2012, 2, 27))
      testDateTime(someDate.minusWeeks(-53))(of(2012, 3, 5))
      testDateTime(leapDate.minusWeeks(53))(of(2011, 2, 23))
      testDateTime(leapDate.minusWeeks(52))(of(2011, 3, 2))
      testDateTime(leapDate.minusWeeks(1))(of(2012, 2, 22))
      testDateTime(leapDate.minusWeeks(-1))(of(2012, 3, 7))
      testDateTime(leapDate.minusWeeks(-52))(of(2013, 2, 27))
      testDateTime(leapDate.minusWeeks(-53))(of(2013, 3, 6))
      testDateTime(MIN.minusWeeks(-104354999947L))(of(999999999, 12, 27))
      testDateTime(MAX.minusWeeks(104354999947L))(of(-999999999, 1, 5))
      intercept[DateTimeException](MIN.minusWeeks(1))
      intercept[DateTimeException](MIN.minusWeeks(-104354999948L))
      intercept[DateTimeException](MAX.minusWeeks(1043549999478L))
      intercept[DateTimeException](MAX.minusWeeks(-1))
      intercept[ArithmeticException](MIN.minusWeeks(Long.MaxValue))
      intercept[ArithmeticException](MAX.minusWeeks(Long.MinValue))
    }

    'test_minusDays - {
      for (d <- samples)
        testDateTime(d.minusDays(0))(d)
      testDateTime(someDate.minusDays(365))(of(2010, 2, 28))
      testDateTime(someDate.minusDays(1))(of(2011, 2, 27))
      testDateTime(someDate.minusDays(-1))(of(2011, 3, 1))
      testDateTime(someDate.minusDays(-365))(of(2012, 2, 28))
      testDateTime(someDate.minusDays(-366))(leapDate)
      testDateTime(leapDate.minusDays(366))(someDate)
      testDateTime(leapDate.minusDays(365))(of(2011, 3, 1))
      testDateTime(leapDate.minusDays(1))(of(2012, 2, 28))
      testDateTime(leapDate.minusDays(-1))(of(2012, 3, 1))
      testDateTime(leapDate.minusDays(-365))(of(2013, 2, 28))
      testDateTime(leapDate.minusDays(-366))(of(2013, 3, 1))
      testDateTime(MIN.minusDays(-730484999633L))(MAX)
      testDateTime(MAX.minusDays(730484999633L))(MIN)
      intercept[DateTimeException](MIN.minusDays(1))
      intercept[DateTimeException](MIN.minusDays(-730484999634L))
      intercept[DateTimeException](MAX.minusDays(730484999634L))
      intercept[DateTimeException](MAX.minusDays(-1))
      intercept[ArithmeticException](ofEpochDay(-2).minusDays(Long.MaxValue))
      intercept[ArithmeticException](ofEpochDay(1).minusDays(Long.MinValue))
    }

    'test_adjustInto - {
      for {
        d1 <- samples
        d2 <- samples
      } {
        testDateTime(d1.adjustInto(d2))(d1)
      }

      val ts = Seq(LocalTime.MIN, LocalTime.MAX)
      for {
        d <- samples
        t <- ts
      } {
        intercept[DateTimeException](d.adjustInto(t))
      }
    }

    'test_until - {
      val samples1 = samples ++ Seq(of(2012, 1, 29), of(2012, 1, 30), of(2012, 2, 28),
          of(2013, 2, 28), of(2013, 3, 1), of(0, 12, 31), of(1, 1, 1))

      for {
        d <- samples1
        u <- dateBasedUnits
      } {
        assert(0L == d.until(d, u))
      }

      assert(730484999633L == MIN.until(MAX, DAYS))
      assert(366L == someDate.until(leapDate, DAYS))
      assert(28L == leapDate.until(of(2012, 3, 28), DAYS))
      assert(104354999947L == MIN.until(MAX, WEEKS))
      assert(12L == someDate.until(leapDate, MONTHS))
      assert(1L == of(2012, 1, 29).until(leapDate, MONTHS))
      assert(0L == of(2012, 1, 30).until(leapDate, MONTHS))
      assert(1999999998L == MIN.until(MAX, YEARS))
      assert(1L == someDate.until(of(2012, 2, 28), YEARS))
      assert(0L == leapDate.until(of(2013, 2, 28), YEARS))
      assert(199999999L == MIN.until(MAX, DECADES))
      assert(19999999L == MIN.until(MAX, CENTURIES))
      assert(1999999L == MIN.until(MAX, MILLENNIA))
      assert(1L == MIN.until(MAX, ERAS))
      assert(1L == of(0, 12, 31).until(of(1, 1, 1), ERAS))

      for {
        d1 <- samples1
        d2 <- samples1 if d2.isAfter(d1)
        u <- dateBasedUnits
      } {
        assert(-d1.until(d2, u) == d2.until(d1, u))
      }

      for (d <- samples1)
        assert(Period.ZERO == d.until(d))

      for {
        d1 <- samples1
        d2 <- samples1
        u <- timeBasedUnits
      } {
        intercept[UnsupportedTemporalTypeException](d1.until(d2, u))
      }

      assert(Period.of(1999999998, 11, 30) == MIN.until(MAX))
      assert(Period.of(-1999999998, -11, -30) == MAX.until(MIN))
      assert(Period.of(1, 0, 1) == someDate.until(leapDate))
      assert(Period.of(-1, 0, -1) == leapDate.until(someDate))
      assert(Period.of(0, 11, 30) == leapDate.until(of(2013, 2, 28)))
      assert(Period.of(0, -11, -28) == of(2013, 2, 28).until(leapDate))
      assert(Period.of(1, 0, 1) == leapDate.until(of(2013, 3, 1)))
      assert(Period.of(-1, 0, -1) == of(2013, 3, 1).until(leapDate))
      assert(Period.of(0, 1, 1) == of(2013, 3, 30).until(of(2013, 5, 1)))
      assert(Period.of(0, -1, -2) == of(2013, 5, 1).until(of(2013, 3, 30)))
      assert(Period.of(0, 1, 1) == of(2013, 3, 31).until(of(2013, 5, 1)))
      assert(Period.of(0, -1, -1) == of(2013, 5, 1).until(of(2013, 3, 31)))
    }

    'test_toEpochDay - {
      assert(-365243219162L == MIN.toEpochDay)
      assert(-1L == of(1969, 12, 31).toEpochDay)
      assert(0L == of(1970, 1, 1).toEpochDay)
      assert(15033L == someDate.toEpochDay)
      assert(15399L == leapDate.toEpochDay)
      assert(365241780471L == MAX.toEpochDay)
    }

    'test_compareTo - {
      assert(0 == MIN.compareTo(MIN))
      assert(MIN.compareTo(someDate) < 0)
      assert(MIN.compareTo(MAX) < 0)
      assert(someDate.compareTo(MIN) > 0)
      assert(0 == someDate.compareTo(someDate))
      assert(someDate.compareTo(MAX) < 0)
      assert(MAX.compareTo(MIN) > 0)
      assert(MAX.compareTo(someDate) > 0)
      assert(0 == MAX.compareTo(MAX))
    }

    'test_isAfter - {
      assert(MIN.isAfter(MIN) == false)
      assert(MIN.isAfter(someDate) == false)
      assert(MIN.isAfter(MAX) == false)
      assert(someDate.isAfter(MIN))
      assert(someDate.isAfter(someDate) == false)
      assert(someDate.isAfter(MAX) == false)
      assert(MAX.isAfter(MIN))
      assert(MAX.isAfter(someDate))
      assert(MAX.isAfter(MAX) == false)
    }

    'test_isBefore - {
      assert(MIN.isBefore(MIN)  == false)
      assert(MIN.isBefore(someDate))
      assert(MIN.isBefore(MAX))
      assert(someDate.isBefore(MIN) == false)
      assert(someDate.isBefore(someDate) == false)
      assert(someDate.isBefore(MAX))
      assert(MAX.isBefore(MIN) == false)
      assert(MAX.isBefore(someDate) == false)
      assert(MAX.isBefore(MAX) == false)
    }

    'test_toString - {
      assert("-999999999-01-01" == MIN.toString)
      assert("-0001-12-31" == of(-1, 12, 31).toString)
      assert("0000-01-01" == of(0, 1, 1).toString)
      assert("2011-02-28" == someDate.toString)
      assert("2012-02-29" == leapDate.toString)
      assert("9999-12-31" == of(9999, 12, 31).toString)
      assert("+10000-01-01" == of(10000, 1, 1).toString)
      assert("+999999999-12-31" == MAX.toString)
    }

    // TODO ScalaNative -> not working ...
    // 'test_now - {
    //   assert(IsoEra.CE == now().getEra)
    // }

    'test_of - {
      val years = Seq(Int.MinValue, -1000000000, -999999999, 0, 999999999,
          1000000000, Int.MaxValue)
      val days = Seq(Int.MinValue, 0, 1, 28, 29, 30, 31, 32, Int.MaxValue)

      for {
        year <- years
        month <- Month.values
        day <- days
      } {
        testDateTime(of(year, month, day))(of(year, month.getValue, day))
      }

      intercept[DateTimeException](of(Int.MinValue, 1, 1))
      intercept[DateTimeException](of(-1000000000, 1, 1))
      intercept[DateTimeException](of(2011, Int.MinValue, 1))
      intercept[DateTimeException](of(2011, 0, 1))
      intercept[DateTimeException](of(2011, 13, 1))
      intercept[DateTimeException](of(2011, Int.MaxValue, 1))

      for (month <- Month.values) {
        val m = month.getValue
        intercept[DateTimeException](of(2011, m, Int.MinValue))
        intercept[DateTimeException](of(2011, m, 0))
        intercept[DateTimeException](of(2011, m, month.length(false) + 1))
        intercept[DateTimeException](of(2012, m, month.length(true) + 1))
        intercept[DateTimeException](of(2011, m, Int.MaxValue))
      }
    }

    'test_ofYearDay - {
      testDateTime(ofYearDay(2011, 1))(of(2011, 1, 1))
      testDateTime(ofYearDay(2011, 31))(of(2011, 1, 31))
      testDateTime(ofYearDay(2011, 32))(of(2011, 2, 1))
      testDateTime(ofYearDay(2011, 59))(of(2011, 2, 28))
      testDateTime(ofYearDay(2011, 60))(of(2011, 3, 1))
      testDateTime(ofYearDay(2011, 90))(of(2011, 3, 31))
      testDateTime(ofYearDay(2011, 91))(of(2011, 4, 1))
      testDateTime(ofYearDay(2011, 120))(of(2011, 4, 30))
      testDateTime(ofYearDay(2011, 121))(of(2011, 5, 1))
      testDateTime(ofYearDay(2011, 151))(of(2011, 5, 31))
      testDateTime(ofYearDay(2011, 152))(of(2011, 6, 1))
      testDateTime(ofYearDay(2011, 181))(of(2011, 6, 30))
      testDateTime(ofYearDay(2011, 182))(of(2011, 7, 1))
      testDateTime(ofYearDay(2011, 212))(of(2011, 7, 31))
      testDateTime(ofYearDay(2011, 213))(of(2011, 8, 1))
      testDateTime(ofYearDay(2011, 243))(of(2011, 8, 31))
      testDateTime(ofYearDay(2011, 244))(of(2011, 9, 1))
      testDateTime(ofYearDay(2011, 273))(of(2011, 9, 30))
      testDateTime(ofYearDay(2011, 274))(of(2011, 10, 1))
      testDateTime(ofYearDay(2011, 304))(of(2011, 10, 31))
      testDateTime(ofYearDay(2011, 305))(of(2011, 11, 1))
      testDateTime(ofYearDay(2011, 334))(of(2011, 11, 30))
      testDateTime(ofYearDay(2011, 335))(of(2011, 12, 1))
      testDateTime(ofYearDay(2011, 365))(of(2011, 12, 31))
      testDateTime(ofYearDay(2012, 1))(of(2012, 1, 1))
      testDateTime(ofYearDay(2012, 31))(of(2012, 1, 31))
      testDateTime(ofYearDay(2012, 32))(of(2012, 2, 1))
      testDateTime(ofYearDay(2012, 60))(of(2012, 2, 29))
      testDateTime(ofYearDay(2012, 61))(of(2012, 3, 1))
      testDateTime(ofYearDay(2012, 91))(of(2012, 3, 31))
      testDateTime(ofYearDay(2012, 92))(of(2012, 4, 1))
      testDateTime(ofYearDay(2012, 121))(of(2012, 4, 30))
      testDateTime(ofYearDay(2012, 122))(of(2012, 5, 1))
      testDateTime(ofYearDay(2012, 152))(of(2012, 5, 31))
      testDateTime(ofYearDay(2012, 153))(of(2012, 6, 1))
      testDateTime(ofYearDay(2012, 182))(of(2012, 6, 30))
      testDateTime(ofYearDay(2012, 183))(of(2012, 7, 1))
      testDateTime(ofYearDay(2012, 213))(of(2012, 7, 31))
      testDateTime(ofYearDay(2012, 214))(of(2012, 8, 1))
      testDateTime(ofYearDay(2012, 244))(of(2012, 8, 31))
      testDateTime(ofYearDay(2012, 245))(of(2012, 9, 1))
      testDateTime(ofYearDay(2012, 274))(of(2012, 9, 30))
      testDateTime(ofYearDay(2012, 275))(of(2012, 10, 1))
      testDateTime(ofYearDay(2012, 305))(of(2012, 10, 31))
      testDateTime(ofYearDay(2012, 306))(of(2012, 11, 1))
      testDateTime(ofYearDay(2012, 335))(of(2012, 11, 30))
      testDateTime(ofYearDay(2012, 336))(of(2012, 12, 1))
      testDateTime(ofYearDay(2012, 366))(of(2012, 12, 31))

      intercept[DateTimeException](ofYearDay(Int.MinValue, 1))
      intercept[DateTimeException](ofYearDay(-1000000000, 1))
      intercept[DateTimeException](ofYearDay(1000000000, 1))
      intercept[DateTimeException](ofYearDay(Int.MaxValue, 1))
      intercept[DateTimeException](ofYearDay(2011, Int.MinValue))
      intercept[DateTimeException](ofYearDay(2011, 0))
      intercept[DateTimeException](ofYearDay(2011, 366))
      intercept[DateTimeException](ofYearDay(2012, 367))
      intercept[DateTimeException](ofYearDay(2011, Int.MaxValue))
    }

    'test_ofEpochDay - {
      testDateTime(ofEpochDay(-365243219162L))(MIN)
      testDateTime(ofEpochDay(-1))(of(1969, 12, 31))
      testDateTime(ofEpochDay(0))(of(1970, 1, 1))
      testDateTime(ofEpochDay(1))(of(1970, 1, 2))
      testDateTime(ofEpochDay(365241780471L))(MAX)

      intercept[DateTimeException](ofEpochDay(Long.MinValue))
      intercept[DateTimeException](ofEpochDay(-365243219163L))
      intercept[DateTimeException](ofEpochDay(365241780472L))
      intercept[DateTimeException](ofEpochDay(Long.MaxValue))
    }

    'test_from - {
      for (d <- samples)
        testDateTime(from(d))(d)

      for (t <- Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX))
        intercept[DateTimeException](from(t))
    }

    'test_parse - {
      assert(parse("-999999999-01-01") == MIN)
      assert(parse("-0001-12-31") == of(-1, 12, 31))
      assert(parse("0000-01-01") == of(0, 1, 1))
      assert(parse("2011-02-28") == someDate)
      assert(parse("2012-02-29") == leapDate)
      assert(parse("9999-12-31") == of(9999, 12, 31))
      assert(parse("+10000-01-01") == of(10000, 1, 1))
      assert(parse("+999999999-12-31") == MAX)

      intercept[DateTimeParseException](parse("0000-01-99"))
      intercept[DateTimeParseException](parse("0000-01-900"))
      intercept[DateTimeParseException](parse("aaaa-01-30"))
      intercept[DateTimeParseException](parse("2012-13-30"))
      intercept[DateTimeParseException](parse("2012-01-34"))
      intercept[DateTimeParseException](parse("2005-02-29"))
    }
  }
}
