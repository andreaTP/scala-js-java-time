package org.scalajs.testsuite.javalib.time.chrono

import java.time.LocalDate
import java.time.chrono.ChronoPeriod

import utest._

object ChronoPeriodTest extends TestSuite {

  val tests = Tests {
    
    'test_between - {
      val ds = Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX)
      for {
        d1 <- ds
        d2 <- ds
      } {
        assert(d1.until(d2) == ChronoPeriod.between(d1, d2))
      }
    }
  }
}
