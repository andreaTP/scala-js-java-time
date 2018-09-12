package org.scalajs.testsuite.javalib.time

import java.time.temporal.{UnsupportedTemporalTypeException, ChronoUnit, TemporalAmount}

import scala.collection.JavaConverters._

import utest._

trait TemporalAmountTest {
  val samples: Seq[TemporalAmount]

  val units: Seq[ChronoUnit]

  val temporalAmountTests = Tests {
    'test_get_unsupported_unit - {
      val illegalUnits = ChronoUnit.values.filterNot(units.contains)
      for {
        amount <- samples
        unit <- illegalUnits
      } {
        intercept[UnsupportedTemporalTypeException](amount.get(unit))
      }
    }

    'test_getUnits - {
      for (amount <- samples)
        assert(units.toIterable == amount.getUnits.asScala)
    }
  }
}
