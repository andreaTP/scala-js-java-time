package org.scalajs.testsuite.javalib.time.chrono

import java.time.DateTimeException
import java.time.chrono.IsoEra
import java.time.temporal.ChronoField

import utest._
import org.scalajs.testsuite.javalib.time.TemporalAccessorTest

object IsoEraTestTemporalAccess extends TestSuite with TemporalAccessorTest[IsoEra] {
  import IsoEra._

  val samples = values.toSeq

  def isSupported(field: ChronoField): Boolean =
    field == ChronoField.ERA

  val tests = temporalAccessorTests
}

object IsoEraTest extends TestSuite {
  import IsoEra._

  val samples = values.toSeq

  val tests = Tests {
  
    'test_getValue - {
      assert(0 == BCE.getValue)
      assert(1 == CE.getValue)
    }

    'test_getLong - {
      for (era <- samples)
        assert(era.getValue.toLong == era.getLong(ChronoField.ERA))
    }

    'test_compareTo - {
      assert(0 == BCE.compareTo(BCE))
      assert(BCE.compareTo(CE) < 0)
      assert(CE.compareTo(BCE) > 0)
      assert(0 == CE.compareTo(CE))
    }

    'test_values - {
      val eras = Seq(BCE, CE)
      assert(eras == values.toSeq)
    }

    'test_valueOf - {
      assert(BCE == valueOf("BCE"))
      assert(CE == valueOf("CE"))
      intercept[IllegalArgumentException](valueOf(""))
    }

    'test_of - {
      assert(BCE == of(0))
      assert(CE == of(1))

      for (n <- Seq(Int.MinValue, -1, 2, Int.MaxValue))
        intercept[DateTimeException](of(n))
    }
  }
}
