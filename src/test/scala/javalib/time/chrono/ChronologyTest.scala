package org.scalajs.testsuite.javalib.time.chrono

import java.time.DateTimeException
import java.time.chrono.{IsoChronology, Chronology}

import utest._

object ChronologyTest extends TestSuite {
  import Chronology._

  val tests = Tests {

    'test_of - {
      assert(IsoChronology.INSTANCE == of("ISO"))
      intercept[DateTimeException](of(""))
    }

    'test_getAvailableChronologies - {
      val chronologies = Chronology.getAvailableChronologies
      assert(chronologies.contains(IsoChronology.INSTANCE))
    }
  }

}
