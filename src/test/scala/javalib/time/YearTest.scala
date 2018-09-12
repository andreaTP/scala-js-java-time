package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal._

import utest._

import scala.annotation.tailrec

/** Created by alonsodomin on 24/12/2015. */
object YearTestUtils {
  import Year._
  import ChronoUnit._
  import ChronoField._
  import DateTimeTestUtil._

  val min = Year.of(MIN_VALUE)
  val max = Year.of(MAX_VALUE)
  val leapYear = Year.of(2016)
  val nonLeapYear = Year.of(2015)
  val lastBCYear = Year.of(0)
  val firstACYear = Year.of(1)
  val negativeYear = Year.of(-100)  
}

object YearTestTemporal extends TestSuite with TemporalTest[Year] {
  import Year._
  import ChronoUnit._
  import ChronoField._
  import DateTimeTestUtil._
  import YearTestUtils._

  val samples = Seq(min, max, leapYear, nonLeapYear, lastBCYear, firstACYear, negativeYear)

  override def isSupported(unit: ChronoUnit): Boolean =
    unit == YEARS || unit == DECADES || unit == CENTURIES || unit == MILLENNIA || unit == ERAS

  override def isSupported(field: ChronoField): Boolean =
    field == YEAR_OF_ERA || field == YEAR || field == ERA

  override def expectedRangeFor(accessor: Year, field: TemporalField): ValueRange = {
    field match {
      case YEAR_OF_ERA =>
        if (accessor.getValue() <= 0) ValueRange.of(1, MAX_VALUE + 1)
        else ValueRange.of(1, MAX_VALUE)

      case _ =>
        super.expectedRangeFor(accessor, field)
    }
  }

  val tests = temporalTests

}

object YearTest extends TestSuite {
  import Year._
  import ChronoUnit._
  import ChronoField._
  import DateTimeTestUtil._
  import YearTestUtils._

  def isSupported(unit: ChronoUnit): Boolean = YearTestTemporal.isSupported(unit)

  def isSupported(field: ChronoField): Boolean = YearTestTemporal.isSupported(field)

  val sampleLongs = YearTestTemporal.sampleLongs

  val samples = Seq(min, max, leapYear, nonLeapYear, lastBCYear, firstACYear, negativeYear)

  val monthDaySamples = Month.values().map(m => MonthDay.of(m, m.minLength()))

  val tests = Tests {

    'getLong - {
      for {
        t <- samples
        field <- ChronoField.values()
      } {
        if (isSupported(field)) {
          val expected = ((field: @unchecked) match {
            case YEAR_OF_ERA => if (t.getValue < 1) 1 - t.getValue else t.getValue
            case YEAR        => t.getValue
            case ERA         => if (t.getValue < 1) 0 else 1
          }).toLong

          assert(expected == t.getLong(field))
        } else {
          intercept[UnsupportedTemporalTypeException](t.getLong(field))
        }
      }

      assert(1L == lastBCYear.getLong(YEAR_OF_ERA))
      assert(0L == lastBCYear.getLong(YEAR))
      assert(0L == lastBCYear.getLong(ERA))

      assert(101L == negativeYear.getLong(YEAR_OF_ERA))
      assert(-100L == negativeYear.getLong(YEAR))
      assert(0L == negativeYear.getLong(ERA))
    }

    'length - {
      assert(366 == leapYear.length())
      assert(365 == nonLeapYear.length())

      for (t <- samples) {
        val expected = if (t.isLeap) 366 else 365
        assert(expected == t.length())
      }
    }

    'with - {
      assert(lastBCYear == lastBCYear.`with`(YEAR, 0))
      assert(Year.of(0) == lastBCYear.`with`(YEAR_OF_ERA, 1))
      assert(Year.of(-50) == lastBCYear.`with`(YEAR, -50))
      assert(Year.of(50) == lastBCYear.`with`(YEAR, 50))
      assert(Year.of(1) == lastBCYear.`with`(ERA, 1))
      assert(lastBCYear == lastBCYear.`with`(ERA, 0))

      assert(firstACYear == firstACYear.`with`(YEAR, 1))
      assert(Year.of(1) == firstACYear.`with`(YEAR_OF_ERA, 1))
      assert(Year.of(-50) == firstACYear.`with`(YEAR, -50))
      assert(Year.of(50) == firstACYear.`with`(YEAR, 50))
      assert(firstACYear == firstACYear.`with`(ERA, 1))
      assert(Year.of(0) == firstACYear.`with`(ERA, 0))

      assert(negativeYear == negativeYear.`with`(YEAR, -100))
      assert(Year.of(0) == negativeYear.`with`(YEAR_OF_ERA, 1))
      assert(Year.of(-50) == negativeYear.`with`(YEAR, -50))
      assert(Year.of(50) == negativeYear.`with`(YEAR, 50))
      assert(negativeYear == negativeYear.`with`(ERA, 0))
      assert(Year.of(101) == negativeYear.`with`(ERA, 1))

      for (t <- samples) {
        intercept[DateTimeException](t.`with`(YEAR, Long.MinValue))
        intercept[DateTimeException](t.`with`(YEAR, -1000000000))
        intercept[DateTimeException](t.`with`(YEAR, 1000000000))
        intercept[DateTimeException](t.`with`(YEAR, Long.MaxValue))

        intercept[DateTimeException](t.`with`(YEAR_OF_ERA, Long.MinValue))
        intercept[DateTimeException](t.`with`(YEAR_OF_ERA, -1000000001))
        intercept[DateTimeException](t.`with`(YEAR_OF_ERA, -1))
        intercept[DateTimeException](t.`with`(YEAR_OF_ERA, 0))
        intercept[DateTimeException](t.`with`(YEAR_OF_ERA, 1000000001))
        intercept[DateTimeException](t.`with`(YEAR_OF_ERA, Long.MaxValue))

        intercept[DateTimeException](t.`with`(ERA, Long.MinValue))
        intercept[DateTimeException](t.`with`(ERA, -1))
        intercept[DateTimeException](t.`with`(ERA, 2))
        intercept[DateTimeException](t.`with`(ERA, Long.MaxValue))
      }
    }

    'plus - {
      for {
        t <- samples
        u <- ChronoUnit.values() if isSupported(u)
      } {
        assert(t == t.plus(0, u))
      }

      assert(Year.of(10) == lastBCYear.plus(1, DECADES))
      assert(Year.of(-10) == lastBCYear.plus(-1, DECADES))
      assert(Year.of(100) == lastBCYear.plus(1, CENTURIES))
      assert(Year.of(-100) == lastBCYear.plus(-1, CENTURIES))
      assert(Year.of(1000) == lastBCYear.plus(1, MILLENNIA))
      assert(Year.of(-1000) == lastBCYear.plus(-1, MILLENNIA))
      assert(firstACYear == lastBCYear.plus(1, ERAS))
      intercept[DateTimeException](lastBCYear.plus(-1, ERAS))

      assert(Year.of(11) == firstACYear.plus(1, DECADES))
      assert(Year.of(-9) == firstACYear.plus(-1, DECADES))
      assert(Year.of(101) == firstACYear.plus(1, CENTURIES))
      assert(Year.of(-99) == firstACYear.plus(-1, CENTURIES))
      assert(Year.of(1001) == firstACYear.plus(1, MILLENNIA))
      assert(Year.of(-999) == firstACYear.plus(-1, MILLENNIA))
      assert(lastBCYear == firstACYear.plus(-1, ERAS))
      intercept[DateTimeException](firstACYear.plus(1, ERAS))

      assert(lastBCYear == min.plus(999999999L, YEARS))
      assert(lastBCYear == max.plus(-999999999L, YEARS))
      assert(max == min.plus(1999999998, YEARS))
      assert(min == max.plus(-1999999998, YEARS))

      for {
        t <- samples
        v <- sampleLongs
      } {
        testDateTime(t.plus(v, YEARS))(t.plusYears(v))
        testDateTime(t.plus(v, DECADES))(t.plusYears(Math.multiplyExact(v, 10)))
        testDateTime(t.plus(v, CENTURIES))(t.plusYears(Math.multiplyExact(v, 100)))
        testDateTime(t.plus(v, MILLENNIA))(t.plusYears(Math.multiplyExact(v, 1000)))
      }
    }

    'plusYears - {
      for (t <- samples) {
        assert(t == t.plusYears(0))
      }

      assert(firstACYear == lastBCYear.plusYears(1))
      assert(lastBCYear == firstACYear.plusYears(-1))
      assert(max == min.plusYears(1999999998))
      assert(min == max.plusYears(-1999999998))

      intercept[DateTimeException](min.plusYears(-1))
      intercept[DateTimeException](min.plusYears(1999999999))
      intercept[DateTimeException](min.plusYears(Long.MinValue))
      intercept[DateTimeException](max.plusYears(1))
      intercept[DateTimeException](max.plusYears(-1999999999))
      intercept[DateTimeException](max.plusYears(Long.MaxValue))
    }

    'minusYears - {
      for (t <- samples) {
        assert(t == t.minusYears(0))
      }

      assert(lastBCYear == firstACYear.minusYears(1))
      assert(firstACYear == lastBCYear.minusYears(-1))
      assert(max == min.minusYears(-1999999998))
      assert(min == max.minusYears(1999999998))

      intercept[DateTimeException](min.minusYears(1))
      intercept[DateTimeException](min.minusYears(-1999999999))
      intercept[DateTimeException](min.minusYears(Long.MinValue))
      intercept[DateTimeException](max.minusYears(-1))
      intercept[DateTimeException](max.minusYears(1999999999))
      intercept[DateTimeException](max.minusYears(Long.MaxValue))
    }

    'adjustInto - {
      val aDate = LocalDate.of(2015, 1, 1)
      assert(LocalDate.of(0, 1, 1) == lastBCYear.adjustInto(aDate))
      assert(LocalDate.of(1, 1, 1) == firstACYear.adjustInto(aDate))
      assert(LocalDate.of(-100, 1, 1) == negativeYear.adjustInto(aDate))
      assert(LocalDate.of(Year.MIN_VALUE, 1, 1) == min.adjustInto(aDate))
      assert(LocalDate.of(Year.MAX_VALUE, 1, 1) == max.adjustInto(aDate))

      val leapDate = LocalDate.of(2012, 2, 29)
      assert(LocalDate.of(2016, 2, 29) == leapYear.adjustInto(leapDate))
      assert(LocalDate.of(2015, 2, 28) == nonLeapYear.adjustInto(leapDate))
    }

    'until - {
      for {
        t <- samples
        unit <- ChronoUnit.values() if isSupported(unit)
      } {
        assert(0L == t.until(t, unit))
      }

      @tailrec def nextLeapYear(year: Int): Year = {
        val nextYearValue = year + 1
        if (Year.isLeap(nextYearValue)) Year.of(nextYearValue)
        else nextLeapYear(year + 1)
      }
      assert(4L == leapYear.until(nextLeapYear(leapYear.getValue), YEARS))

      assert(10L == lastBCYear.until(Year.of(10), YEARS))
      assert(-10L == lastBCYear.until(Year.of(-10), YEARS))
      assert(10L == firstACYear.until(Year.of(11), YEARS))
      assert(-12L == firstACYear.until(Year.of(-11), YEARS))
      assert(1L == lastBCYear.until(Year.of(10), DECADES))
      assert(-1L == lastBCYear.until(Year.of(-10), DECADES))
      assert(1L == firstACYear.until(Year.of(11), DECADES))
      assert(-1L == firstACYear.until(Year.of(-11), DECADES))
      assert(0L == lastBCYear.until(Year.of(10), CENTURIES))
      assert(0L == lastBCYear.until(Year.of(-10), CENTURIES))
      assert(0L == firstACYear.until(Year.of(11), CENTURIES))
      assert(0L == firstACYear.until(Year.of(-11), CENTURIES))
      assert(0L == lastBCYear.until(Year.of(10), MILLENNIA))
      assert(0L == lastBCYear.until(Year.of(-10), MILLENNIA))
      assert(0L == firstACYear.until(Year.of(11), MILLENNIA))
      assert(0L == firstACYear.until(Year.of(-11), MILLENNIA))

      assert(100L == lastBCYear.until(Year.of(100), YEARS))
      assert(-100L == lastBCYear.until(Year.of(-100), YEARS))
      assert(100L == firstACYear.until(Year.of(101), YEARS))
      assert(-102L == firstACYear.until(Year.of(-101), YEARS))
      assert(10L == lastBCYear.until(Year.of(100), DECADES))
      assert(-10L == lastBCYear.until(Year.of(-100), DECADES))
      assert(10L == firstACYear.until(Year.of(101), DECADES))
      assert(-10L == firstACYear.until(Year.of(-101), DECADES))
      assert(1L == lastBCYear.until(Year.of(100), CENTURIES))
      assert(-1L == lastBCYear.until(Year.of(-100), CENTURIES))
      assert(1L == firstACYear.until(Year.of(101), CENTURIES))
      assert(-1L == firstACYear.until(Year.of(-101), CENTURIES))
      assert(0L == lastBCYear.until(Year.of(100), MILLENNIA))
      assert(0L == lastBCYear.until(Year.of(-100), MILLENNIA))
      assert(0L == firstACYear.until(Year.of(101), MILLENNIA))
      assert(0L == firstACYear.until(Year.of(-101), MILLENNIA))

      assert(1000L == lastBCYear.until(Year.of(1000), YEARS))
      assert(-1000L == lastBCYear.until(Year.of(-1000), YEARS))
      assert(1000L == firstACYear.until(Year.of(1001), YEARS))
      assert(-1002L == firstACYear.until(Year.of(-1001), YEARS))
      assert(100L == lastBCYear.until(Year.of(1000), DECADES))
      assert(-100L == lastBCYear.until(Year.of(-1000), DECADES))
      assert(100L == firstACYear.until(Year.of(1001), DECADES))
      assert(-100L == firstACYear.until(Year.of(-1001), DECADES))
      assert(10L == lastBCYear.until(Year.of(1000), CENTURIES))
      assert(-10L == lastBCYear.until(Year.of(-1000), CENTURIES))
      assert(10L == firstACYear.until(Year.of(1001), CENTURIES))
      assert(-10L == firstACYear.until(Year.of(-1001), CENTURIES))
      assert(1L == lastBCYear.until(Year.of(1000), MILLENNIA))
      assert(-1L == lastBCYear.until(Year.of(-1000), MILLENNIA))
      assert(1L == firstACYear.until(Year.of(1001), MILLENNIA))
      assert(-1L == firstACYear.until(Year.of(-1001), MILLENNIA))

      assert(1L == lastBCYear.until(firstACYear, ERAS))
      assert(-1L == firstACYear.until(lastBCYear, ERAS))
      assert(0L == firstACYear.until(Year.of(100), ERAS))
      assert(0L == lastBCYear.until(Year.of(-100), ERAS))
      assert(1L == negativeYear.until(Year.of(100), ERAS))

      assert(1999999998L == min.until(max, YEARS))
      assert(-1999999998L == max.until(min, YEARS))
      assert(199999999L == min.until(max, DECADES))
      assert(-199999999L == max.until(min, DECADES))
      assert(19999999L == min.until(max, CENTURIES))
      assert(-19999999L == max.until(min, CENTURIES))
      assert(1999999L == min.until(max, MILLENNIA))
      assert(-1999999L == max.until(min, MILLENNIA))
      assert(1L == min.until(max, ERAS))
      assert(-1L == max.until(min, ERAS))
    }

    'atDay - {
      for (t <- samples) {
        assert(LocalDate.of(t.getValue, 1, 1) == t.atDay(1))
        assert(LocalDate.of(t.getValue, 12, 31) == t.atDay(t.length()))
        intercept[DateTimeException](t.atDay(-1))
      }

      assert(LocalDate.of(leapYear.getValue, 2, 29) == leapYear.atDay(31 + 29))
      assert(LocalDate.of(leapYear.getValue, 3, 1) == leapYear.atDay(31 + 30))
      assert(LocalDate.of(leapYear.getValue, 12, 30) == leapYear.atDay(365))
      assert(LocalDate.of(leapYear.getValue, 12, 31) == leapYear.atDay(366))
      intercept[DateTimeException](leapYear.atDay(367))

      assert(LocalDate.of(nonLeapYear.getValue, 3, 1) == nonLeapYear.atDay(31 + 29))
      assert(LocalDate.of(nonLeapYear.getValue, 12, 31) == nonLeapYear.atDay(365))
      intercept[DateTimeException](nonLeapYear.atDay(366))
    }

    'atMonth - {
      for {
        t <- samples
        month <- Month.values()
      } {
        assert(YearMonth.of(t.getValue, month.getValue) == t.atMonth(month))
        assert(YearMonth.of(t.getValue, month.getValue) == t.atMonth(month.getValue))
        intercept[DateTimeException](t.atMonth(0))
        intercept[DateTimeException](t.atMonth(13))
      }
    }

    'isLeap - {
      assert(leapYear.isLeap)
      assert(nonLeapYear.isLeap == false)

      assert(Year.isLeap(leapYear.getValue))
      assert(Year.isLeap(nonLeapYear.getValue) == false)
    }

    'isValidMonthDay - {
      val leapMonthDay = MonthDay.of(2, 29)
      assert(leapYear.isValidMonthDay(leapMonthDay))
      assert(nonLeapYear.isValidMonthDay(leapMonthDay) == false)

      for {
        t <- samples
        monthDay <- monthDaySamples
      } {
        assert(t.isValidMonthDay(monthDay))
      }
    }

    'atMonthDay - {
      val leapMonthDay = MonthDay.of(2, 29)
      val leapDate = LocalDate.of(leapYear.getValue(),
          leapMonthDay.getMonthValue, leapMonthDay.getDayOfMonth)
      assert(leapDate == leapYear.atMonthDay(leapMonthDay))

      val nonLeapDate = LocalDate.of(nonLeapYear.getValue(),
          leapMonthDay.getMonthValue, 28)
      assert(nonLeapDate == nonLeapYear.atMonthDay(leapMonthDay))

      for {
        t <- samples
        monthDay <- monthDaySamples
      } {
        val expected = LocalDate.of(t.getValue, monthDay.getMonthValue, monthDay.getDayOfMonth)
        assert(expected == t.atMonthDay(monthDay))
      }
    }

    'compareTo - {
      assert(0 == min.compareTo(min))
      assert(-1999999998 == min.compareTo(max))
      assert(1999999998 == max.compareTo(min))
      assert(0 == max.compareTo(max))

      assert(999999999 == lastBCYear.compareTo(min))
      assert(-999999999 == min.compareTo(lastBCYear))
      assert(1000000000 == firstACYear.compareTo(min))
      assert(-1000000000 == min.compareTo(firstACYear))

      assert(-1 == lastBCYear.compareTo(firstACYear))
      assert(1 == firstACYear.compareTo(lastBCYear))
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

    // TODO: Fix in Scala Native
    // 'now - {
    //   val now = LocalDate.now()
    //   val year = Year.now()

    //   if (now.getYear != year.getValue) {
    //     assert(LocalDate.now().getYear == year.getValue)
    //     println("Happy new year " + year + '!')
    //   }
    // }

    'of - {
      assert(lastBCYear == Year.of(0))
      assert(0 == Year.of(0).getValue)

      assert(firstACYear == Year.of(1))
      assert(1 == Year.of(1).getValue)

      assert(min == Year.of(MIN_VALUE))
      assert(-999999999 == Year.of(MIN_VALUE).getValue)

      assert(max == Year.of(MAX_VALUE))
      assert(999999999 == Year.of(MAX_VALUE).getValue)
    }

    'from - {
      for (t <- samples)
        assert(t == Year.from(t))

      val dateSamples = Seq(LocalDate.MIN, LocalDate.MAX, LocalDate.of(0, 1, 1))
      for (d <- dateSamples) {
        assert(Year.of(d.getYear) == Year.from(d))
      }
    }

    'toStringOutput - {
      for (t <- samples) {
        val expected = t.getValue.toString
        assert(expected == t.toString)
      }
    }
  }
}
