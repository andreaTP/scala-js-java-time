package org.scalajs.testsuite.javalib.time

import java.time.temporal._

import utest._

trait TemporalTest[Temp <: Temporal] extends TemporalAccessorTest[Temp] {
  import DateTimeTestUtil._

  def isSupported(unit: ChronoUnit): Boolean

  val sampleLongs = Seq(
      Long.MinValue, Int.MinValue.toLong, -1000000000L, -86400L,
      -3600L, -366L, -365L, -60L, -24L, -7L, -1L, 0L,
      1L, 7L, 24L, 60L, 365L, 366L, 3600L, 86400L, 1000000000L,
      Int.MaxValue.toLong, Long.MaxValue)

  val temporalTests = Tests {

    'isSupported_TemporalUnit - {
      for {
        temporal <- samples
        unit <- ChronoUnit.values
      } {
        if (isSupported(unit))
          assert(temporal.isSupported(unit))
        else
          assert(temporal.isSupported(unit) == false)
      }
      for (temporal <- samples)
        assert(temporal.isSupported(null: TemporalUnit) == false)
    }

    'with_unsupported_field - {
      for {
        temporal <- samples
        field <- ChronoField.values if !temporal.isSupported(field)
        n <- sampleLongs.filter(field.range.isValidValue)
      } {
        intercept[UnsupportedTemporalTypeException](temporal.`with`(field, n))
      }
    }

    'plus_unsupported_unit - {
      for {
        temporal <- samples
        unit <- ChronoUnit.values if !temporal.isSupported(unit)
        n <- sampleLongs
      } {
        intercept[UnsupportedTemporalTypeException](temporal.plus(n, unit))
      }
    }

    'minus - {
      for {
        temporal <- samples
        unit <- ChronoUnit.values if temporal.isSupported(unit)
        n <- sampleLongs
      } {
        testDateTime(temporal.minus(n, unit)) {
          if (n != Long.MinValue) temporal.plus(-n, unit)
          else temporal.plus(Long.MaxValue, unit).plus(1, unit)
        }
      }
    }

    'minus_unsupported_unit - {
      for {
        temporal <- samples
        unit <- ChronoUnit.values if !temporal.isSupported(unit)
        n <- sampleLongs
      } {
        intercept[UnsupportedTemporalTypeException](temporal.minus(n, unit))
      }
    }

    'until_unsupported_unit - {
      for {
        temporal1 <- samples
        temporal2 <- samples
        unit <- ChronoUnit.values if !temporal1.isSupported(unit)
      } {
        intercept[UnsupportedTemporalTypeException](temporal1.until(temporal2, unit))
      }
    }
  }
}
