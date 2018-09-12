package org.scalajs.testsuite.javalib.time

import java.time.DateTimeException
import java.time.temporal._

import utest._

trait TemporalAccessorTest[TempAcc <: TemporalAccessor] {
  val samples: Seq[TempAcc]

  def isSupported(field: ChronoField): Boolean

  def expectedRangeFor(accessor: TempAcc, field: TemporalField): ValueRange = field.range()

  val temporalAccessorTests = Tests { 
    'isSupported_TemporalField - {
      for {
        accessor <- samples
        field <- ChronoField.values
      } {
        if (isSupported(field))
          assert(accessor.isSupported(field))
        else
          assert(accessor.isSupported(field) == false)
      }
      for (accessor <- samples)
        assert(accessor.isSupported(null) == false)
    }

    // def expectedRangeFor(accessor: TempAcc, field: TemporalField): ValueRange = field.range()

    'range - {
      for {
        accessor <- samples
        field <- ChronoField.values
      } {
        if (accessor.isSupported(field)) {
          val expected = expectedRangeFor(accessor, field)
          assert(expected == accessor.range(field))
        } else {
          intercept[UnsupportedTemporalTypeException](accessor.range(field))
        }
      }
    }

    'get - {
      for {
        accessor <- samples
        field <- ChronoField.values
      } {
        if (accessor.isSupported(field) && field.range.isIntValue)
          assert(accessor.getLong(field) == accessor.get(field).toLong)
        else if (accessor.isSupported(field))
          intercept[DateTimeException](accessor.get(field))
        else
          intercept[UnsupportedTemporalTypeException](accessor.get(field))
      }
    }

    'getLong_unsupported_field - {
      for {
        accessor <- samples
        field <- ChronoField.values() if !accessor.isSupported(field)
      } {
        intercept[UnsupportedTemporalTypeException](accessor.getLong(field))
      }
    }
  }
}
