package org.scalajs.testsuite.javalib.time.temporal

import java.time.temporal.ChronoField

import utest._

object ChronoFieldTest extends TestSuite {
  import ChronoField._

  val tests = Tests {
    'test_isDateBased - {
      assert(NANO_OF_SECOND.isDateBased == false)
      assert(NANO_OF_DAY.isDateBased == false)
      assert(MICRO_OF_SECOND.isDateBased == false)
      assert(MICRO_OF_DAY.isDateBased == false)
      assert(MILLI_OF_SECOND.isDateBased == false)
      assert(MILLI_OF_DAY.isDateBased == false)
      assert(SECOND_OF_MINUTE.isDateBased == false)
      assert(SECOND_OF_DAY.isDateBased == false)
      assert(MINUTE_OF_HOUR.isDateBased == false)
      assert(MINUTE_OF_DAY.isDateBased == false)
      assert(HOUR_OF_AMPM.isDateBased == false)
      assert(CLOCK_HOUR_OF_AMPM.isDateBased == false)
      assert(HOUR_OF_DAY.isDateBased == false)
      assert(CLOCK_HOUR_OF_DAY.isDateBased == false)
      assert(AMPM_OF_DAY.isDateBased == false)
      assert(DAY_OF_WEEK.isDateBased)
      assert(ALIGNED_DAY_OF_WEEK_IN_MONTH.isDateBased)
      assert(ALIGNED_DAY_OF_WEEK_IN_YEAR.isDateBased)
      assert(DAY_OF_MONTH.isDateBased)
      assert(DAY_OF_YEAR.isDateBased)
      assert(EPOCH_DAY.isDateBased)
      assert(ALIGNED_WEEK_OF_MONTH.isDateBased)
      assert(ALIGNED_WEEK_OF_YEAR.isDateBased)
      assert(MONTH_OF_YEAR.isDateBased)
      assert(PROLEPTIC_MONTH.isDateBased)
      assert(YEAR_OF_ERA.isDateBased)
      assert(YEAR.isDateBased)
      assert(ERA.isDateBased)
      assert(INSTANT_SECONDS.isDateBased == false)
      assert(OFFSET_SECONDS.isDateBased == false)
    }

    'test_isTimeBased - {
      assert(NANO_OF_SECOND.isTimeBased)
      assert(NANO_OF_DAY.isTimeBased)
      assert(MICRO_OF_SECOND.isTimeBased)
      assert(MICRO_OF_DAY.isTimeBased)
      assert(MILLI_OF_SECOND.isTimeBased)
      assert(MILLI_OF_DAY.isTimeBased)
      assert(SECOND_OF_MINUTE.isTimeBased)
      assert(SECOND_OF_DAY.isTimeBased)
      assert(MINUTE_OF_HOUR.isTimeBased)
      assert(MINUTE_OF_DAY.isTimeBased)
      assert(HOUR_OF_AMPM.isTimeBased)
      assert(CLOCK_HOUR_OF_AMPM.isTimeBased)
      assert(HOUR_OF_DAY.isTimeBased)
      assert(CLOCK_HOUR_OF_DAY.isTimeBased)
      assert(AMPM_OF_DAY.isTimeBased)
      assert(DAY_OF_WEEK.isTimeBased == false)
      assert(ALIGNED_DAY_OF_WEEK_IN_MONTH.isTimeBased == false)
      assert(ALIGNED_DAY_OF_WEEK_IN_YEAR.isTimeBased == false)
      assert(DAY_OF_MONTH.isTimeBased == false)
      assert(DAY_OF_YEAR.isTimeBased == false)
      assert(EPOCH_DAY.isTimeBased == false)
      assert(ALIGNED_WEEK_OF_MONTH.isTimeBased == false)
      assert(ALIGNED_WEEK_OF_YEAR.isTimeBased == false)
      assert(MONTH_OF_YEAR.isTimeBased == false)
      assert(PROLEPTIC_MONTH.isTimeBased == false)
      assert(YEAR_OF_ERA.isTimeBased == false)
      assert(YEAR.isTimeBased == false)
      assert(ERA.isTimeBased == false)
      assert(INSTANT_SECONDS.isTimeBased == false)
      assert(OFFSET_SECONDS.isTimeBased == false)
    }

    'test_values - {
      val fields = Seq[AnyRef](NANO_OF_SECOND, NANO_OF_DAY, MICRO_OF_SECOND,
          MICRO_OF_DAY, MILLI_OF_SECOND, MILLI_OF_DAY, SECOND_OF_MINUTE,
          SECOND_OF_DAY, MINUTE_OF_HOUR, MINUTE_OF_DAY, HOUR_OF_AMPM,
          CLOCK_HOUR_OF_AMPM, HOUR_OF_DAY, CLOCK_HOUR_OF_DAY, AMPM_OF_DAY,
          DAY_OF_WEEK, ALIGNED_DAY_OF_WEEK_IN_MONTH, ALIGNED_DAY_OF_WEEK_IN_YEAR,
          DAY_OF_MONTH, DAY_OF_YEAR, EPOCH_DAY, ALIGNED_WEEK_OF_MONTH,
          ALIGNED_WEEK_OF_YEAR, MONTH_OF_YEAR, PROLEPTIC_MONTH, YEAR_OF_ERA, YEAR,
          ERA, INSTANT_SECONDS, OFFSET_SECONDS)
      assert(fields == values.toSeq)
    }

    'test_valueOf - {
      assert(NANO_OF_SECOND == valueOf("NANO_OF_SECOND"))
      assert(NANO_OF_DAY == valueOf("NANO_OF_DAY"))
      assert(MICRO_OF_SECOND == valueOf("MICRO_OF_SECOND"))
      assert(MICRO_OF_DAY == valueOf("MICRO_OF_DAY"))
      assert(MILLI_OF_SECOND == valueOf("MILLI_OF_SECOND"))
      assert(MILLI_OF_DAY == valueOf("MILLI_OF_DAY"))
      assert(SECOND_OF_MINUTE == valueOf("SECOND_OF_MINUTE"))
      assert(SECOND_OF_DAY == valueOf("SECOND_OF_DAY"))
      assert(MINUTE_OF_HOUR == valueOf("MINUTE_OF_HOUR"))
      assert(MINUTE_OF_DAY == valueOf("MINUTE_OF_DAY"))
      assert(HOUR_OF_AMPM == valueOf("HOUR_OF_AMPM"))
      assert(CLOCK_HOUR_OF_AMPM == valueOf("CLOCK_HOUR_OF_AMPM"))
      assert(HOUR_OF_DAY == valueOf("HOUR_OF_DAY"))
      assert(CLOCK_HOUR_OF_DAY == valueOf("CLOCK_HOUR_OF_DAY"))
      assert(AMPM_OF_DAY == valueOf("AMPM_OF_DAY"))
      assert(DAY_OF_WEEK == valueOf("DAY_OF_WEEK"))
      assert(ALIGNED_DAY_OF_WEEK_IN_MONTH == valueOf("ALIGNED_DAY_OF_WEEK_IN_MONTH"))
      assert(ALIGNED_DAY_OF_WEEK_IN_YEAR == valueOf("ALIGNED_DAY_OF_WEEK_IN_YEAR"))
      assert(DAY_OF_MONTH == valueOf("DAY_OF_MONTH"))
      assert(DAY_OF_YEAR == valueOf("DAY_OF_YEAR"))
      assert(EPOCH_DAY == valueOf("EPOCH_DAY"))
      assert(ALIGNED_WEEK_OF_MONTH == valueOf("ALIGNED_WEEK_OF_MONTH"))
      assert(ALIGNED_WEEK_OF_YEAR == valueOf("ALIGNED_WEEK_OF_YEAR"))
      assert(MONTH_OF_YEAR == valueOf("MONTH_OF_YEAR"))
      assert(PROLEPTIC_MONTH == valueOf("PROLEPTIC_MONTH"))
      assert(YEAR_OF_ERA == valueOf("YEAR_OF_ERA"))
      assert(YEAR == valueOf("YEAR"))
      assert(ERA == valueOf("ERA"))
      assert(INSTANT_SECONDS == valueOf("INSTANT_SECONDS"))
      assert(OFFSET_SECONDS == valueOf("OFFSET_SECONDS"))
    }
  }
}
