package org.scalajs.testsuite.javalib.time.chrono

import java.time.{Period, LocalDate, DateTimeException}
import java.time.chrono.{IsoChronology, IsoEra}
import java.time.temporal.ChronoField

import utest._
import org.scalajs.testsuite.javalib.time.DateTimeTestUtil._

object IsoChronologyTest extends TestSuite {
  final val iso = IsoChronology.INSTANCE

  val tests = Tests {
    'test_getId - {
      assert("ISO" == iso.getId)
    }

    'test_getCalendarType - {
      assert("iso8601" == iso.getCalendarType)
    }

    'test_date - {
      val years = Seq(Int.MinValue, -1000000000, -999999999, -1, 0,
          1, 999999999, 1000000000, Int.MaxValue)
      val months = Seq(Int.MinValue, 0, 1, 12, 13, Int.MaxValue)
      val days = Seq(Int.MinValue, 0, 1, 28, 29, 30, 31, 32, Int.MaxValue)

      for {
        year <- years
        month <- months
        day <- days
      } {
        testDateTime(iso.date(IsoEra.CE, year, month, day))(LocalDate.of(year, month, day))
        testDateTime(iso.date(IsoEra.BCE, 1 - year, month, day))(LocalDate.of(year, month, day))
        testDateTime(iso.date(year, month, day))(LocalDate.of(year, month, day))
        intercept[ClassCastException](iso.date(null, year, month, day))
      }
    }

    'test_dateYearDay - {
      val years = Seq(Int.MinValue, -1000000000, -999999999, -1, 0,
          1, 999999999, 1000000000, Int.MaxValue)
      val months = Seq(Int.MinValue, 0, 1, 12, 13, Int.MaxValue)
      val days = Seq(Int.MinValue, 0, 1, 365, 366, Int.MaxValue)

      for {
        year <- years
        day <- days
      } {
        testDateTime(iso.dateYearDay(IsoEra.CE, year, day))(LocalDate.ofYearDay(year, day))
        testDateTime(iso.dateYearDay(IsoEra.BCE, 1 - year, day))(LocalDate.ofYearDay(year, day))
        testDateTime(iso.dateYearDay(year, day))(LocalDate.ofYearDay(year, day))
        intercept[ClassCastException](iso.dateYearDay(null, year, day))
      }
    }

    'test_dateEpochDay - {
      for (day <- Seq(Long.MinValue, -365243219163L, -365243219162L, -1L, 0L,
          1L, 365241780471L, 365241780472L, Long.MaxValue)) {
        testDateTime(iso.dateEpochDay(day))(LocalDate.ofEpochDay(day))
      }
    }

    // 'test_dateNow - {
    //   assert(IsoEra.CE == iso.dateNow().getEra)
    // }

    'test_isLeapYear - {
      for (year <- Seq(Int.MinValue, -400, -104, -96, -4, 0, 4, 1896, 1904,
          1996, 2000, 2004, 2147483644)) {
        assert(iso.isLeapYear(year))
      }
      for (year <- Seq(-2147483647, -100, -99, -1, 1, 1900, 1999, 2001, 2002,
          2003, 2005, Int.MaxValue)) {
        assert(iso.isLeapYear(year) == false)
      }
    }

    'test_prolepticYear - {
      for (year <- Seq(-Int.MinValue, -1, 0, 1, Int.MaxValue)) {
        assert(year == iso.prolepticYear(IsoEra.CE, year))
        assert(1 - year == iso.prolepticYear(IsoEra.BCE, year))
      }
    }

    'test_eraOf - {
      assert(IsoEra.BCE == iso.eraOf(0))
      assert(IsoEra.CE == iso.eraOf(1))

      for (n <- Seq(-Int.MinValue, -1, 2, Int.MaxValue))
        intercept[DateTimeException](iso.eraOf(n))
    }

    'test_eras - {
      val eras = iso.eras
      assert(2 == eras.size)
      assert(IsoEra.BCE == eras.get(0))
      assert(IsoEra.CE == eras.get(1))
    }

    'test_range - {
      for (f <- ChronoField.values)
        assert(f.range == iso.range(f))
    }

    'test_period - {
      val yearss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
      val monthss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
      val dayss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)

      for {
        years <- yearss
        months <- monthss
        days <- dayss
      } {
        assert(Period.of(years, months, days) == iso.period(years, months, days))
      }
    }

    'test_toString - {
      assert("ISO" == iso.toString)
    }
  }
}
