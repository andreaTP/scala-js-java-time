package org.scalajs.testsuite.javalib.time.chrono

import java.time.DateTimeException
import java.time.chrono.{IsoChronology, Chronology}

import utest._

object ChronologyTest extends TestSuite {
  import Chronology._

  val tests = Tests {

    'test_of - {
      assert(IsoChronology.INSTANCE == of("ISO"))
    }

  }
  // @Test def test_of(): Unit = {
  //   assertEquals(IsoChronology.INSTANCE, of("ISO"))
  //   expectThrows(classOf[DateTimeException], of(""))
  // }

  // @Test def test_getAvailableChronologies(): Unit = {
  //   val chronologies = Chronology.getAvailableChronologies
  //   assertTrue(chronologies.contains(IsoChronology.INSTANCE))
  // }
}
