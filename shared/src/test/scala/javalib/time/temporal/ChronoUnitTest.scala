package org.scalajs.testsuite.javalib.time.temporal

import java.time.temporal.ChronoUnit

import utest._

object ChronoUnitTest extends TestSuite {
  import ChronoUnit._

  val tests = Tests {
    'test_isDurationEstimated - {
      for (u <- ChronoUnit.values)
        assert(u.isDurationEstimated != u.isTimeBased)
    }

    'test_isDateBased - {
      assert(NANOS.isDateBased == false)
      assert(MICROS.isDateBased == false)
      assert(MILLIS.isDateBased == false)
      assert(SECONDS.isDateBased == false)
      assert(MINUTES.isDateBased == false)
      assert(HOURS.isDateBased == false)
      assert(HALF_DAYS.isDateBased == false)
      assert(DAYS.isDateBased)
      assert(WEEKS.isDateBased)
      assert(MONTHS.isDateBased)
      assert(YEARS.isDateBased)
      assert(DECADES.isDateBased)
      assert(CENTURIES.isDateBased)
      assert(MILLENNIA.isDateBased)
      assert(ERAS.isDateBased)
      assert(FOREVER.isDateBased == false)
    }

    'test_isTimeBased - {
      assert(NANOS.isTimeBased)
      assert(MICROS.isTimeBased)
      assert(MILLIS.isTimeBased)
      assert(SECONDS.isTimeBased)
      assert(MINUTES.isTimeBased)
      assert(HOURS.isTimeBased)
      assert(HALF_DAYS.isTimeBased)
      assert(DAYS.isTimeBased == false)
      assert(WEEKS.isTimeBased == false)
      assert(MONTHS.isTimeBased == false)
      assert(YEARS.isTimeBased == false)
      assert(DECADES.isTimeBased == false)
      assert(CENTURIES.isTimeBased == false)
      assert(MILLENNIA.isTimeBased == false)
      assert(ERAS.isTimeBased == false)
      assert(FOREVER.isTimeBased == false)
    }

    'test_values - {
      val units = Seq(NANOS, MICROS, MILLIS, SECONDS, MINUTES, HOURS,
          HALF_DAYS, DAYS, WEEKS, MONTHS, YEARS, DECADES, CENTURIES, MILLENNIA,
          ERAS, FOREVER)
      assert(units == values.toSeq)
    }

    'test_valueOf - {
      assert(NANOS == valueOf("NANOS"))
      assert(MICROS == valueOf("MICROS"))
      assert(MILLIS == valueOf("MILLIS"))
      assert(SECONDS == valueOf("SECONDS"))
      assert(MINUTES == valueOf("MINUTES"))
      assert(HOURS == valueOf("HOURS"))
      assert(HALF_DAYS == valueOf("HALF_DAYS"))
      assert(DAYS == valueOf("DAYS"))
      assert(WEEKS == valueOf("WEEKS"))
      assert(MONTHS == valueOf("MONTHS"))
      assert(YEARS == valueOf("YEARS"))
      assert(DECADES == valueOf("DECADES"))
      assert(CENTURIES == valueOf("CENTURIES"))
      assert(MILLENNIA == valueOf("MILLENNIA"))
      assert(ERAS == valueOf("ERAS"))
      assert(FOREVER == valueOf("FOREVER"))
    }
  }
}
