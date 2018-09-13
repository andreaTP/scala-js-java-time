package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.{ChronoUnit, UnsupportedTemporalTypeException}

import utest._

object DurationTestUtil {

  import DateTimeTestUtil._
  import Duration._
  import ChronoUnit._

  final val dmin = Duration.ofSeconds(Long.MinValue)
  final val dmax = Duration.ofSeconds(Long.MaxValue, 999999999)
  final val oneSecond = Duration.ofSeconds(1)
  final val oneNano = Duration.ofNanos(1)

}

object DurationTestTemporalAmount extends TestSuite with TemporalAmountTest {
  
  import DateTimeTestUtil._
  import Duration._
  import ChronoUnit._
  import DurationTestUtil._

  val samples =
    Seq(dmin, dmax, ZERO, oneSecond, oneSecond.negated, oneNano.negated)

  val units = Seq(SECONDS, NANOS)

  val tests = temporalAmountTests
}

object DurationTest extends TestSuite {

  import DateTimeTestUtil._
  import Duration._
  import ChronoUnit._
  import DurationTestUtil._

  val samples =
    Seq(dmin, dmax, ZERO, oneSecond, oneSecond.negated, oneNano.negated)

  val units = Seq(SECONDS, NANOS)

  val illegalUnits =
    ChronoUnit.values.filterNot(_.isTimeBased).filterNot(_ == DAYS)

  val tests = Tests {
    'test_get - {
      for (d <- samples) {
        assert(d.getSeconds == d.get(SECONDS))
        assert(d.getNano.toLong == d.get(NANOS))
      }

      'test_isZero - {
        for (d <- samples if d != ZERO)
          assert(d.isZero == false)
        assert(ZERO.isZero)
      }

      'test_isNegative - {
        assert(dmin.isNegative)
        assert(oneSecond.negated.isNegative)
        assert(oneNano.negated.isNegative)
        assert(ZERO.isNegative == false)
        assert(oneNano.isNegative == false)
        assert(oneSecond.isNegative == false)
        assert(dmax.isNegative == false)
      }

      'test_getSeconds - {
        assert(Long.MinValue == dmin.getSeconds)
        assert(-1L == oneNano.negated.getSeconds)
        assert(0L == ZERO.getSeconds)
        assert(0L == ofSeconds(1, -1).getSeconds)
        assert(1L == oneSecond.getSeconds)
        assert(Long.MaxValue == dmax.getSeconds)
      }

      'test_getNano - {
        assert(0 == dmin.getNano)
        assert(999999999 == oneNano.negated.getNano)
        assert(0 == ZERO.getNano)
        assert(1 == oneNano.getNano)
        assert(999999999 == dmax.getNano)
      }
    }
    
    
    'test_withSeconds - {
      assert(ZERO == dmin.withSeconds(0))
      assert(ofSeconds(2, -1) == dmax.withSeconds(1))
      assert(dmin == ZERO.withSeconds(Long.MinValue))
    }

    'test_withNanos - {
      val d0 = ofSeconds(1, 1)

      assert(ofSeconds(Long.MinValue + 1, -1) == dmin.withNanos(999999999))
      assert(oneSecond == d0.withNanos(0))
      assert(ofSeconds(2, -1) == d0.withNanos(999999999))
      assert(ofSeconds(Long.MaxValue) == dmax.withNanos(0))

      val args = Seq(Int.MinValue, -1, 1000000000, Int.MaxValue)
      for {
        d <- samples
        n <- args
      } {
        intercept[DateTimeException](d.withNanos(n))
      }
    }

    'test_plus - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(1, 999999999)
      val d3 = ofSeconds(-2, 1)

      for (d <- samples) {
        assert(d == d.plus(ZERO))
        assert(d == ZERO.plus(d))
      }
      assert(ofSeconds(2, 2) == d1.plus(d1))
      assert(ofSeconds(3) == d1.plus(d2))
      assert(ofSeconds(-1, 2) == d1.plus(d3))
      intercept[ArithmeticException](dmax.plus(oneNano))
      intercept[ArithmeticException](dmin.plus(oneNano.negated))

      val args = Seq(Long.MinValue, -100000000000000L, 1L, 0L, 1L,
          100000000000000L, Long.MaxValue)
      for {
        d <- samples
        n <- args
      } {
        testDateTime(d.plus(n, NANOS))(d.plusNanos(n))
        testDateTime(d.plus(n, MICROS))(d.plus(ofNanos(1000).multipliedBy(n)))
        testDateTime(d.plus(n, MILLIS))(d.plusMillis(n))
        testDateTime(d.plus(n, SECONDS))(d.plusSeconds(n))
        testDateTime(d.plus(n, MINUTES))(d.plusMinutes(n))
        testDateTime(d.plus(n, HOURS))(d.plusHours(n))
        testDateTime(d.plus(n, HALF_DAYS))(d.plus(ofHours(12).multipliedBy(n)))
        testDateTime(d.plus(n, DAYS))(d.plusDays(n))
      }
      for {
        d <- samples
        n <- args
        u <- illegalUnits
      } {
        intercept[UnsupportedTemporalTypeException](d.plus(n, u))
      }
    }

    'test_plusDays - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 86400, 999999999)
      val d3 = ofSeconds(Long.MinValue + 86400)
      val d4 = ofSeconds(Long.MaxValue - 86399)
      val d5 = ofSeconds(Long.MinValue + 86400, -1)

      assert(ofSeconds(-431999, 1) == d1.plusDays(-5))
      assert(ofSeconds(-86399, 1) == d1.plusDays(-1))
      assert(d1 == d1.plusDays(0))
      assert(ofSeconds(86401, 1) == d1.plusDays(1))
      assert(ofSeconds(432001, 1) == d1.plusDays(5))
      assert(dmax == d2.plusDays(1))
      assert(dmin == d3.plusDays(-1))
      assert(dmax == dmax.plusDays(0))
      assert(d2 == dmax.plusDays(-1))
      assert(dmin == dmin.plusDays(0))
      assert(d3 == dmin.plusDays(1))

      intercept[ArithmeticException](d4.plusDays(1))
      intercept[ArithmeticException](d2.plusDays(2))
      intercept[ArithmeticException](d5.plusDays(-1))
      intercept[ArithmeticException](d3.plusDays(-2))
    }

    'test_plusHours - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 3600, 999999999)
      val d3 = ofSeconds(Long.MinValue + 3600)
      val d4 = ofSeconds(Long.MaxValue - 3599)
      val d5 = ofSeconds(Long.MinValue + 3600, -1)

      assert(ofSeconds(-17999, 1) == d1.plusHours(-5))
      assert(ofSeconds(-3599, 1) == d1.plusHours(-1))
      assert(d1 == d1.plusHours(0))
      assert(ofSeconds(3601, 1) == d1.plusHours(1))
      assert(ofSeconds(18001, 1) == d1.plusHours(5))
      assert(dmax == d2.plusHours(1))
      assert(dmin == d3.plusHours(-1))
      assert(dmax == dmax.plusHours(0))
      assert(d2 == dmax.plusHours(-1))
      assert(dmin == dmin.plusHours(0))
      assert(d3 == dmin.plusHours(1))

      intercept[ArithmeticException](d4.plusHours(1))
      intercept[ArithmeticException](d2.plusHours(2))
      intercept[ArithmeticException](d5.plusHours(-1))
      intercept[ArithmeticException](d3.plusHours(-2))
    }

    'test_plusMinutes - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 60, 999999999)
      val d3 = ofSeconds(Long.MinValue + 60)
      val d4 = ofSeconds(Long.MaxValue - 59)
      val d5 = ofSeconds(Long.MinValue + 60, -1)

      assert(ofSeconds(-299, 1) == d1.plusMinutes(-5))
      assert(ofSeconds(-59, 1) == d1.plusMinutes(-1))
      assert(d1 == d1.plusMinutes(0))
      assert(ofSeconds(61, 1) == d1.plusMinutes(1))
      assert(ofSeconds(301, 1) == d1.plusMinutes(5))
      assert(dmax == d2.plusMinutes(1))
      assert(dmin == d3.plusMinutes(-1))
      assert(dmax == dmax.plusMinutes(0))
      assert(d2 == dmax.plusMinutes(-1))
      assert(dmin == dmin.plusMinutes(0))
      assert(d3 == dmin.plusMinutes(1))

      intercept[ArithmeticException](d4.plusMinutes(1))
      intercept[ArithmeticException](d2.plusMinutes(2))
      intercept[ArithmeticException](d5.plusMinutes(-1))
      intercept[ArithmeticException](d3.plusMinutes(-2))
    }

    'test_plusSeconds - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 1, 999999999)
      val d3 = ofSeconds(Long.MinValue + 1)
      val d4 = ofSeconds(Long.MaxValue)
      val d5 = ofSeconds(Long.MinValue + 1, -1)

      assert(ofSeconds(-4, 1) == d1.plusSeconds(-5))
      assert(ofSeconds(0, 1) == d1.plusSeconds(-1))
      assert(d1 == d1.plusSeconds(0))
      assert(ofSeconds(2, 1) == d1.plusSeconds(1))
      assert(ofSeconds(6, 1) == d1.plusSeconds(5))
      assert(dmax == d2.plusSeconds(1))
      assert(dmin == d3.plusSeconds(-1))
      assert(dmax == dmax.plusSeconds(0))
      assert(d2 == dmax.plusSeconds(-1))
      assert(dmin == dmin.plusSeconds(0))
      assert(d3 == dmin.plusSeconds(1))

      intercept[ArithmeticException](d4.plusSeconds(1))
      intercept[ArithmeticException](d2.plusSeconds(2))
      intercept[ArithmeticException](d5.plusSeconds(-1))
      intercept[ArithmeticException](d3.plusSeconds(-2))
    }

    'test_plusMillis - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 998999999)
      val d3 = ofSeconds(Long.MinValue, 1000000)
      val d4 = ofSeconds(Long.MaxValue, 999000000)
      val d5 = ofSeconds(Long.MinValue, 999999)

      assert(ofSeconds(-4, 1) == d1.plusMillis(-5000))
      assert(ofSeconds(0, 900000001) == d1.plusMillis(-100))
      assert(d1 == d1.plusMillis(0))
      assert(ofSeconds(1, 100000001) == d1.plusMillis(100))
      assert(ofSeconds(6, 1) == d1.plusMillis(5000))
      assert(dmax == d2.plusMillis(1))
      assert(dmin == d3.plusMillis(-1))
      assert(dmax == dmax.plusMillis(0))
      assert(d2 == dmax.plusMillis(-1))
      assert(dmin == dmin.plusMillis(0))
      assert(d3 == dmin.plusMillis(1))

      intercept[ArithmeticException](d4.plusMillis(1))
      intercept[ArithmeticException](d2.plusMillis(2))
      intercept[ArithmeticException](d5.plusMillis(-1))
      intercept[ArithmeticException](d3.plusMillis(-2))
    }

    'test_plusNanos - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 999999998)
      val d3 = ofSeconds(Long.MinValue, 1)

      assert(ofSeconds(-4, 1) == d1.plusNanos(-5000000000L))
      assert(ofSeconds(0, 999999001) == d1.plusNanos(-1000))
      assert(d1 == d1.plusNanos(0))
      assert(ofSeconds(1, 1001) == d1.plusNanos(1000))
      assert(ofSeconds(6, 1) == d1.plusNanos(5000000000L))
      assert(dmax == d2.plusNanos(1))
      assert(dmin == d3.plusNanos(-1))
      assert(dmax == dmax.plusNanos(0))
      assert(d2 == dmax.plusNanos(-1))
      assert(dmin == dmin.plusNanos(0))
      assert(d3 == dmin.plusNanos(1))

      intercept[ArithmeticException](dmax.plusNanos(1))
      intercept[ArithmeticException](d2.plusNanos(2))
      intercept[ArithmeticException](dmin.plusNanos(-1))
      intercept[ArithmeticException](d3.plusNanos(-2))
    }

    'test_minus - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(1, 999999999)
      val d3 = ofSeconds(-2, 1)

      assert(ZERO == d1.minus(d1))
      assert(d1 == d1.minus(ZERO))
      assert(d1.negated == ZERO.minus(d1))
      assert(ofSeconds(-1, 2) == d1.minus(d2))
      assert(ofSeconds(3) == d1.minus(d3))
      intercept[ArithmeticException](dmax.minus(oneNano.negated))
      intercept[ArithmeticException](dmin.minus(oneNano))

      val args = Seq(Long.MinValue, -100000000000000L, 1L, 0L, 1L,
          100000000000000L, Long.MaxValue)
      for {
        d <- samples
        n <- args
      } {
        testDateTime(d.minus(n, NANOS))(d.minusNanos(n))
        testDateTime(d.minus(n, MICROS))(d.minus(ofNanos(1000).multipliedBy(n)))
        testDateTime(d.minus(n, MILLIS))(d.minusMillis(n))
        testDateTime(d.minus(n, SECONDS))(d.minusSeconds(n))
        testDateTime(d.minus(n, MINUTES))(d.minusMinutes(n))
        testDateTime(d.minus(n, HOURS))(d.minusHours(n))
        testDateTime(d.minus(n, HALF_DAYS))(d.minus(ofHours(12).multipliedBy(n)))
        testDateTime(d.minus(n, DAYS))(d.minusDays(n))
      }
      for {
        d <- samples
        n <- args
        u <- illegalUnits
      } {
        intercept[UnsupportedTemporalTypeException](d.minus(n, u))
      }
    }

    'test_minusDays - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 86400, 999999999)
      val d3 = ofSeconds(Long.MinValue + 86400)
      val d4 = ofSeconds(Long.MaxValue - 86399)
      val d5 = ofSeconds(Long.MinValue + 86400, -1)

      assert(ofSeconds(-431999, 1) == d1.minusDays(5))
      assert(ofSeconds(-86399, 1) == d1.minusDays(1))
      assert(d1 == d1.minusDays(0))
      assert(ofSeconds(86401, 1) == d1.minusDays(-1))
      assert(ofSeconds(432001, 1) == d1.minusDays(-5))
      assert(dmax == d2.minusDays(-1))
      assert(dmin == d3.minusDays(1))
      assert(dmax == dmax.minusDays(0))
      assert(d2 == dmax.minusDays(1))
      assert(dmin == dmin.minusDays(0))
      assert(d3 == dmin.minusDays(-1))

      intercept[ArithmeticException](d4.minusDays(-1))
      intercept[ArithmeticException](d2.minusDays(-2))
      intercept[ArithmeticException](d5.minusDays(1))
      intercept[ArithmeticException](d3.minusDays(2))
    }

    'test_minusHours - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 3600, 999999999)
      val d3 = ofSeconds(Long.MinValue + 3600)
      val d4 = ofSeconds(Long.MaxValue - 3599)
      val d5 = ofSeconds(Long.MinValue + 3600, -1)

      assert(ofSeconds(-17999, 1) == d1.minusHours(5))
      assert(ofSeconds(-3599, 1) == d1.minusHours(1))
      assert(d1 == d1.minusHours(0))
      assert(ofSeconds(3601, 1) == d1.minusHours(-1))
      assert(ofSeconds(18001, 1) == d1.minusHours(-5))
      assert(dmax == d2.minusHours(-1))
      assert(dmin == d3.minusHours(1))
      assert(dmax == dmax.minusHours(0))
      assert(d2 == dmax.minusHours(1))
      assert(dmin == dmin.minusHours(0))
      assert(d3 == dmin.minusHours(-1))

      intercept[ArithmeticException](d4.minusHours(-1))
      intercept[ArithmeticException](d2.minusHours(-2))
      intercept[ArithmeticException](d5.minusHours(1))
      intercept[ArithmeticException](d3.minusHours(2))
    }

    'test_minusMinutes - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 60, 999999999)
      val d3 = ofSeconds(Long.MinValue + 60)
      val d4 = ofSeconds(Long.MaxValue - 59)
      val d5 = ofSeconds(Long.MinValue + 60, -1)

      assert(ofSeconds(-299, 1) == d1.minusMinutes(5))
      assert(ofSeconds(-59, 1) == d1.minusMinutes(1))
      assert(d1 == d1.minusMinutes(0))
      assert(ofSeconds(61, 1) == d1.minusMinutes(-1))
      assert(ofSeconds(301, 1) == d1.minusMinutes(-5))
      assert(dmax == d2.minusMinutes(-1))
      assert(dmin == d3.minusMinutes(1))
      assert(dmax == dmax.minusMinutes(0))
      assert(d2 == dmax.minusMinutes(1))
      assert(dmin == dmin.minusMinutes(0))
      assert(d3 == dmin.minusMinutes(-1))

      intercept[ArithmeticException](d4.minusMinutes(-1))
      intercept[ArithmeticException](d2.minusMinutes(-2))
      intercept[ArithmeticException](d5.minusMinutes(1))
      intercept[ArithmeticException](d3.minusMinutes(2))
    }

    'test_minusSeconds - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 1, 999999999)
      val d3 = ofSeconds(Long.MinValue + 1)
      val d4 = ofSeconds(Long.MaxValue)
      val d5 = ofSeconds(Long.MinValue + 1, -1)

      assert(ofSeconds(-4, 1) == d1.minusSeconds(5))
      assert(ofSeconds(0, 1) == d1.minusSeconds(1))
      assert(d1 == d1.minusSeconds(0))
      assert(ofSeconds(2, 1) == d1.minusSeconds(-1))
      assert(ofSeconds(6, 1) == d1.minusSeconds(-5))
      assert(dmax == d2.minusSeconds(-1))
      assert(dmin == d3.minusSeconds(1))
      assert(dmax == dmax.minusSeconds(0))
      assert(d2 == dmax.minusSeconds(1))
      assert(dmin == dmin.minusSeconds(0))
      assert(d3 == dmin.minusSeconds(-1))

      intercept[ArithmeticException](d4.minusSeconds(-1))
      intercept[ArithmeticException](d2.minusSeconds(-2))
      intercept[ArithmeticException](d5.minusSeconds(1))
      intercept[ArithmeticException](d3.minusSeconds(2))
    }

    'test_minusMillis - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 998999999)
      val d3 = ofSeconds(Long.MinValue, 1000000)
      val d4 = ofSeconds(Long.MaxValue, 999000000)
      val d5 = ofSeconds(Long.MinValue, 999999)

      assert(ofSeconds(-4, 1) == d1.minusMillis(5000))
      assert(ofSeconds(0, 900000001) == d1.minusMillis(100))
      assert(d1 == d1.minusMillis(0))
      assert(ofSeconds(1, 100000001) == d1.minusMillis(-100))
      assert(ofSeconds(6, 1) == d1.minusMillis(-5000))
      assert(dmax == d2.minusMillis(-1))
      assert(dmin == d3.minusMillis(1))
      assert(dmax == dmax.minusMillis(0))
      assert(d2 == dmax.minusMillis(1))
      assert(dmin == dmin.minusMillis(0))
      assert(d3 == dmin.minusMillis(-1))

      intercept[ArithmeticException](d4.minusMillis(-1))
      intercept[ArithmeticException](d2.minusMillis(-2))
      intercept[ArithmeticException](d5.minusMillis(1))
      intercept[ArithmeticException](d3.minusMillis(2))
    }

    'test_minusNanos - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 999999998)
      val d3 = ofSeconds(Long.MinValue, 1)

      assert(ofSeconds(-4, 1) == d1.minusNanos(5000000000L))
      assert(ofSeconds(0, 999999001) == d1.minusNanos(1000))
      assert(d1 == d1.minusNanos(0))
      assert(ofSeconds(1, 1001) == d1.minusNanos(-1000))
      assert(ofSeconds(6, 1) == d1.minusNanos(-5000000000L))
      assert(dmax == d2.minusNanos(-1))
      assert(dmin == d3.minusNanos(1))
      assert(dmax == dmax.minusNanos(0))
      assert(d2 == dmax.minusNanos(1))
      assert(dmin == dmin.minusNanos(0))
      assert(d3 == dmin.minusNanos(-1))

      intercept[ArithmeticException](dmax.minusNanos(-1))
      intercept[ArithmeticException](d2.minusNanos(-2))
      intercept[ArithmeticException](dmin.minusNanos(1))
      intercept[ArithmeticException](d3.minusNanos(2))
    }

    'test_multipliedBy - {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(-1, -1)

      for (d <- samples) {
        assert(ZERO == d.multipliedBy(0))
        assert(d == d.multipliedBy(1))
        testDateTime(d.multipliedBy(-1))(d.negated)
      }
      for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue))
        assert(ZERO == ZERO.multipliedBy(n))
      assert(d2 == d1.multipliedBy(-1))
      assert(ofSeconds(2, 2) == d1.multipliedBy(2))
      assert(ofSeconds(-2, -2) == d1.multipliedBy(-2))
      assert(ofSeconds(1000000001) == d1.multipliedBy(1000000000))
      assert(ofSeconds(-1000000001) == d1.multipliedBy(-1000000000))
      assert(d1 == d2.multipliedBy(-1))
      assert(ofSeconds(-2, -2) == d2.multipliedBy(2))
      assert(ofSeconds(2, 2) == d2.multipliedBy(-2))
      assert(ofSeconds(-1000000001) == d2.multipliedBy(1000000000))
      assert(ofSeconds(1000000001) == d2.multipliedBy(-1000000000))
      assert(dmin.plusNanos(1) == dmax.multipliedBy(-1))

      intercept[ArithmeticException](dmin.multipliedBy(-1))
      intercept[ArithmeticException](dmin.multipliedBy(2))
      intercept[ArithmeticException](dmax.multipliedBy(2))
      intercept[ArithmeticException](d1.multipliedBy(Long.MaxValue))
      intercept[ArithmeticException](d1.multipliedBy(Long.MinValue))
    }

    'test_dividedBy - {
      val d1 = ofSeconds(10, 100)
      val d2 = ofNanos(10)
      val d3 = Duration.ofSeconds(9223372036L, 999999999)

      for (d <- samples) {
        assert(d == d.dividedBy(1))
        testDateTime(d.dividedBy(-1))(d.negated)
      }
      intercept[ArithmeticException](dmin.dividedBy(-1))
      for (n <- Seq(Long.MinValue, -1L, 1L, Long.MaxValue))
        assert(ZERO == ZERO.dividedBy(n))
      assert(ofSeconds(5, 50) == d1.dividedBy(2))
      assert(ofSeconds(-5, -50) == d1.dividedBy(-2))
      assert(ofSeconds(3, 333333366) == d1.dividedBy(3))
      assert(ofSeconds(-3, -333333366) == d1.dividedBy(-3))
      assert(ofSeconds(1, 10) == d1.dividedBy(10))
      assert(ofSeconds(-1, -10) == d1.dividedBy(-10))
      assert(ofNanos(100000001) == d1.dividedBy(100))
      assert(ofNanos(-100000001) == d1.dividedBy(-100))
      assert(ofMillis(10) == d1.dividedBy(1000))
      assert(d1.dividedBy(-1000) == ofMillis(-10))
      assert(ofNanos(3333333) == d1.dividedBy(3000))
      assert(ofNanos(-3333333) == d1.dividedBy(-3000))
      assert(ofNanos(10) == d1.dividedBy(1000000000))
      assert(ofNanos(-10) == d1.dividedBy(-1000000000))
      assert(oneNano == d1.dividedBy(10000000000L))
      assert(oneNano.negated == d1.dividedBy(-10000000000L))
      assert(oneNano == d1.dividedBy(10000000100L))
      assert(oneNano.negated == d1.dividedBy(-10000000100L))
      assert(ZERO == d1.dividedBy(10000000101L))
      assert(ZERO == d1.dividedBy(-10000000101L))
      assert(oneNano == d2.dividedBy(10))
      assert(oneNano.negated == d2.dividedBy(-10))
      assert(ZERO == d2.dividedBy(11))
      assert(ZERO == d2.dividedBy(-11))
      assert(oneNano == d3.dividedBy(Long.MaxValue))
      assert(oneNano.negated == d3.dividedBy(Long.MinValue))
      assert(oneSecond == dmin.dividedBy(Long.MinValue))
      assert(oneSecond.negated == dmin.dividedBy(Long.MaxValue))
      assert(ofSeconds(-1, 1) == dmax.dividedBy(Long.MinValue))
      assert(oneSecond == dmax.dividedBy(Long.MaxValue))
      assert(ofSeconds(1, -1) == dmin.plusNanos(1).dividedBy(Long.MinValue))
      assert(oneSecond.negated == dmin.plusNanos(1).dividedBy(Long.MaxValue))
      assert(oneSecond.negated == dmin.plusNanos(2).dividedBy(Long.MaxValue))
      assert(oneSecond == dmax.minusNanos(1).dividedBy(Long.MaxValue))

      intercept[ArithmeticException](dmin.dividedBy(-1))
      // TODO: divide by 0 crash the test runner in Scala Native
      // for (d <- samples)
      //   intercept[ArithmeticException](d.dividedBy(0))
    }

    'test_negated - {
      assert(ZERO == ZERO.negated)
      assert(ofSeconds(-1) == oneSecond.negated)
      assert(oneSecond == ofSeconds(-1).negated)
      assert(ofSeconds(-1, -1) == ofSeconds(1, 1).negated)
      assert(ofSeconds(1, 1) == ofSeconds(-1, -1).negated)
      assert(ofSeconds(Long.MinValue, 1) == dmax.negated)
      assert(dmax == ofSeconds(Long.MinValue, 1).negated)

      intercept[ArithmeticException](dmin.negated)
    }

    'test_abs - {
      assert(ZERO == ZERO.abs)
      assert(oneSecond == oneSecond.abs)
      assert(oneSecond == ofSeconds(-1).abs)
      assert(ofSeconds(1, 1) == ofSeconds(1, 1).abs)
      assert(ofSeconds(1, 1) == ofSeconds(-1, -1).abs)
      assert(dmax == dmax.abs)
      assert(dmax == ofSeconds(Long.MinValue, 1).abs)

      intercept[ArithmeticException](dmin.abs)
    }

    'test_addTo - {
      val t = LocalTime.NOON
      val d = LocalDate.MIN

      assert(t == ZERO.addTo(t))
      assert(LocalTime.of(20, 29, 52) == dmin.addTo(t))
      assert(LocalTime.of(3, 30, 7, 999999999) == dmax.addTo(t))
      assert(d == ZERO.addTo(d))

      intercept[UnsupportedTemporalTypeException](oneNano.addTo(d))
      intercept[UnsupportedTemporalTypeException](oneSecond.addTo(d))
    }

    'test_subtractFrom - {
      val t = LocalTime.NOON
      val d = LocalDate.MIN

      assert(t == ZERO.subtractFrom(t))
      assert(LocalTime.of(3, 30, 8) == dmin.subtractFrom(t))
      assert(LocalTime.of(20, 29, 52, 1) == dmax.subtractFrom(t))
      assert(d == ZERO.subtractFrom(d))

      intercept[UnsupportedTemporalTypeException](oneNano.subtractFrom(d))
      intercept[UnsupportedTemporalTypeException](oneSecond.subtractFrom(d))
    }

    'test_toDays - {
      assert(-106751991167300L == dmin.toDays)
      assert(-2L == ofSeconds(-172799, -1).toDays)
      assert(-1L == ofSeconds(-172799).toDays)
      assert(-1L == ofSeconds(-86400).toDays)
      assert(-1L == ofSeconds(-86399, -1).toDays)
      assert(0L == ofSeconds(-86399).toDays)
      assert(0L == ZERO.toDays)
      assert(0L == ofSeconds(86399).toDays)
      assert(0L == ofSeconds(86400, -1).toDays)
      assert(1L == ofSeconds(86400).toDays)
      assert(1L == ofSeconds(172800, -1).toDays)
      assert(2L == ofSeconds(172800).toDays)
      assert(106751991167300L == dmax.toDays)
    }

    'test_toHours - {
      assert(-2562047788015215L == dmin.toHours)
      assert(-2L == ofSeconds(-7199, -1).toHours)
      assert(-1L == ofSeconds(-7199).toHours)
      assert(-1L == ofSeconds(-3600).toHours)
      assert(-1L == ofSeconds(-3599, -1).toHours)
      assert(0L == ofSeconds(-3599).toHours)
      assert(0L == ZERO.toHours)
      assert(0L == ofSeconds(3599).toHours)
      assert(0L == ofSeconds(3600, -1).toHours)
      assert(1L == ofSeconds(3600).toHours)
      assert(1L == ofSeconds(7200, -1).toHours)
      assert(2L == ofSeconds(7200).toHours)
      assert(2562047788015215L == dmax.toHours)
    }

    'test_toMinutes - {
      assert(-153722867280912930L == dmin.toMinutes)
      assert(-2L == ofSeconds(-119, -1).toMinutes)
      assert(-1L == ofSeconds(-119).toMinutes)
      assert(-1L == ofSeconds(-60).toMinutes)
      assert(-1L == ofSeconds(-59, -1).toMinutes)
      assert(0L == ofSeconds(-59).toMinutes)
      assert(0L == ZERO.toMinutes)
      assert(0L == ofSeconds(59).toMinutes)
      assert(0L == ofSeconds(60, -1).toMinutes)
      assert(1L == ofSeconds(60).toMinutes)
      assert(1L == ofSeconds(120, -1).toMinutes)
      assert(2L == ofSeconds(120).toMinutes)
      assert(153722867280912930L == dmax.toMinutes)
    }

    'test_toMillis - {
      assert(-9223372036854775000L == ofSeconds(-9223372036854775L).toMillis)
      assert(-1000L == ofSeconds(-1).toMillis)
      assert(-2L == ofNanos(-1000001).toMillis)
      assert(-1L == ofNanos(-1000000).toMillis)
      assert(-1L == ofNanos(-1).toMillis)
      assert(0L == ZERO.toMillis)
      assert(0L == ofNanos(999999).toMillis)
      assert(1L == ofNanos(1000000).toMillis)
      assert(1L == ofNanos(1999999).toMillis)
      assert(2L == ofNanos(2000000).toMillis)
      assert(1000L == ofSeconds(1).toMillis)
      assert(9223372036854775807L == ofSeconds(9223372036854775L, 807999999).toMillis)

      intercept[ArithmeticException](dmin.toMillis)
      intercept[ArithmeticException](dmax.toMillis)
      // this could yield a valid long, but the reference implementation throws
      intercept[ArithmeticException](ofSeconds(-9223372036854775L, -1).toMillis)
      intercept[ArithmeticException](ofSeconds(9223372036854775L, 808000000).toMillis)
    }

    'test_toNanos - {
      assert(-9223372036000000000L == ofSeconds(-9223372036L).toNanos)
      assert(Int.MinValue.toLong == ofNanos(Int.MinValue).toNanos)
      assert(-1000L == ofNanos(-1000).toNanos)
      assert(-1L == ofNanos(-1).toNanos)
      assert(0L == ofNanos(0).toNanos)
      assert(1L == ofNanos(1).toNanos)
      assert(1000L == ofNanos(1000).toNanos)
      assert(Int.MaxValue.toLong == ofNanos(Int.MaxValue).toNanos)
      assert(Long.MaxValue == ofSeconds(9223372036L, 854775807).toNanos)

      intercept[ArithmeticException](dmin.toNanos)
      intercept[ArithmeticException](dmax.toNanos)
      // this should yield a valid long, but the reference implementation throws
      intercept[ArithmeticException](ofSeconds(-9223372036L, -1).toNanos)
      intercept[ArithmeticException](ofSeconds(9223372036L, 854775808).toNanos)
    }

    'test_compareTo - {
      val d1 = ofSeconds(0, -1)
      val d0 = ZERO
      val d2 = ofSeconds(0, 1)

      assert(0 == dmin.compareTo(dmin))
      assert(dmin.compareTo(d1) < 0)
      assert(dmin.compareTo(d0) < 0)
      assert(dmin.compareTo(d2) < 0)
      assert(dmin.compareTo(dmax) < 0)
      assert(d1.compareTo(dmin) > 0)
      assert(0 == d1.compareTo(d1))
      assert(d1.compareTo(d0) < 0)
      assert(d1.compareTo(d2) < 0)
      assert(d1.compareTo(dmax) < 0)
      assert(d0.compareTo(dmin) > 0)
      assert(d0.compareTo(d1) > 0)
      assert(0 == d0.compareTo(d0))
      assert(d0.compareTo(d2) < 0)
      assert(d0.compareTo(dmax) < 0)
      assert(d2.compareTo(dmin) > 0)
      assert(d2.compareTo(d1) > 0)
      assert(d2.compareTo(d0) > 0)
      assert(0 == d2.compareTo(d2))
      assert(d2.compareTo(dmax) < 0)
      assert(dmax.compareTo(dmin) > 0)
      assert(dmax.compareTo(d1) > 0)
      assert(dmax.compareTo(d0) > 0)
      assert(dmax.compareTo(d2) > 0)
      assert(0 == dmax.compareTo(dmax))
    }

    'test_toString - {
      assert("PT0S" == ZERO.toString)
      assert("PT-0.999999999S" == ofSeconds(-1, 1).toString)
      assert("PT-1.000000001S" == ofSeconds(-1, -1).toString)
      assert("PT1M" == ofSeconds(60).toString)
      assert("PT-1M" == ofSeconds(-60).toString)
      assert("PT59.999999999S" == ofSeconds(60, -1).toString)
      assert("PT1M0.000000001S" == ofSeconds(60, 1).toString)
      assert("PT-1M-0.999999999S" == ofSeconds(-61, 1).toString)
      assert("PT2M0.00000001S" == ofSeconds(120, 10).toString)
      // if (!executingInJVM) // JDK incorrectly prints "PT-2M0.00000001S"
      //   assert("PT-1M-59.99999999S", ofSeconds(-120, 10).toString)
      assert("PT1H" == ofSeconds(3600).toString)
      assert("PT-1H" == ofSeconds(-3600).toString)
      assert("PT-2562047788015215H-30M-8S" == dmin.toString)
      assert("PT2562047788015215H30M7.999999999S" == dmax.toString)
    }

    'test_ofDays - {
      val maxDays = 106751991167300L
      val maxSecs = maxDays * 86400

      assert(ofSeconds(-maxSecs) == ofDays(-maxDays))
      assert(ofSeconds(-86400) == ofDays(-1))
      assert(ZERO == ofDays(0))
      assert(ofSeconds(86400) == ofDays(1))
      assert(ofSeconds(maxSecs) == ofDays(maxDays))

      intercept[ArithmeticException](ofDays(-maxDays - 1))
      intercept[ArithmeticException](ofDays(maxDays + 1))
    }

    'test_ofHours - {
      val maxHrs = 2562047788015215L
      val maxSecs = maxHrs * 3600

      assert(ofSeconds(-maxSecs) == ofHours(-maxHrs))
      assert(ofSeconds(-3600) == ofHours(-1))
      assert(ZERO == ofHours(0))
      assert(ofSeconds(3600) == ofHours(1))
      assert(ofSeconds(maxSecs) == ofHours(maxHrs))

      intercept[ArithmeticException](ofHours(-maxHrs - 1))
      intercept[ArithmeticException](ofHours(maxHrs + 1))
    }

    'test_ofMinutes - {
      val maxMins = 153722867280912930L
      val maxSecs = maxMins * 60

      assert(ofSeconds(-maxSecs) == ofMinutes(-maxMins))
      assert(ofSeconds(-60) == ofMinutes(-1))
      assert(ZERO == ofMinutes(0))
      assert(ofSeconds(60) == ofMinutes(1))
      assert(ofSeconds(maxSecs) == ofMinutes(maxMins))

      intercept[ArithmeticException](ofMinutes(-maxMins - 1))
      intercept[ArithmeticException](ofMinutes(maxMins + 1))
    }

    'test_ofSeconds - {
      assert(ofSeconds(-11, 999999999) == ofSeconds(-10, -1))
      assert(ofSeconds(-1) == ofSeconds(-1, 0))
      assert(ZERO == ofSeconds(-1, 1000000000))
      assert(ZERO == ofSeconds(0))
      assert(ZERO == ofSeconds(0, 0))
      assert(ZERO == ofSeconds(1, -1000000000))
      assert(ofSeconds(1) == ofSeconds(1, 0))
      assert(ofSeconds(9, 999999999) == ofSeconds(10, -1))

      intercept[ArithmeticException](ofSeconds(Long.MinValue, -1))
      intercept[ArithmeticException](ofSeconds(Long.MaxValue, 1000000000))
    }

    'test_ofMillis - {
      assert(ofSeconds(-9223372036854776L, 192000000) == ofMillis(Long.MinValue))
      assert(ofSeconds(-1) == ofMillis(-1000))
      assert(ofSeconds(0, -1000000) == ofMillis(-1))
      assert(ZERO == ofMillis(0))
      assert(ofSeconds(0, 1000000) == ofMillis(1))
      assert(ofSeconds(1) == ofMillis(1000))
      assert(ofSeconds(9223372036854775L, 807000000) == ofMillis(Long.MaxValue))
    }

    'test_ofNanos - {
      assert(ofSeconds(-9223372037L, 145224192) == ofNanos(Long.MinValue))
      assert(ofSeconds(-1) == ofNanos(-1000000000))
      assert(ofSeconds(0, -1) == ofNanos(-1))
      assert(ZERO == ofNanos(0))
      assert(ofSeconds(0, 1) == ofNanos(1))
      assert(ofSeconds(1) == ofNanos(1000000000))
      assert(ofSeconds(9223372036L, 854775807) == ofNanos(Long.MaxValue))
    }

    'test_of - {
      for (n <- Seq(-100000000000000L, -1L, 0L, 1L, 100000000000000L)) {
        assert(ofNanos(n) == of(n, NANOS))
        assert(ofNanos(n * 1000) == of(n, MICROS))
        assert(ofMillis(n) == of(n, MILLIS))
        assert(ofSeconds(n) == of(n, SECONDS))
        assert(ofMinutes(n) == of(n, MINUTES))
        assert(ofHours(n) == of(n, HOURS))
        assert(ofHours(n * 12) == of(n, HALF_DAYS))
        assert(ofDays(n) == of(n, DAYS))
      }
      assert(ofSeconds(-9223372036855L, 224192000) == of(Long.MinValue, MICROS))
      assert(ofSeconds(9223372036854L, 775807000) == of(Long.MaxValue, MICROS))

      for (s <- Seq(-1, 1)) {
        intercept[ArithmeticException](of(106751991167301L * s, DAYS))
        intercept[ArithmeticException](of(213503982334602L * s, HALF_DAYS))
        intercept[ArithmeticException](of(2562047788015216L * s, HOURS))
        intercept[ArithmeticException](of(153722867280912931L * s, MINUTES))
      }

      for (n <- Seq(-1L, 0L, 1L)) {
        intercept[UnsupportedTemporalTypeException](of(n, WEEKS))
        intercept[UnsupportedTemporalTypeException](of(n, MONTHS))
        intercept[UnsupportedTemporalTypeException](of(n, YEARS))
        intercept[UnsupportedTemporalTypeException](of(n, DECADES))
        intercept[UnsupportedTemporalTypeException](of(n, CENTURIES))
        intercept[UnsupportedTemporalTypeException](of(n, MILLENNIA))
        intercept[UnsupportedTemporalTypeException](of(n, ERAS))
        intercept[UnsupportedTemporalTypeException](of(n, FOREVER))
      }
    }

    'test_from - {
      assert(dmin == from(dmin))
      assert(ZERO == from(ZERO))
      assert(dmax == from(dmax))

      intercept[UnsupportedTemporalTypeException](from(Period.ZERO))
    }

    'test_between - {
      val MIN = LocalTime.MIN
      val MAX = LocalTime.MAX

      assert(ofNanos(86399999999999L) == between(MIN, MAX))
      assert(ofNanos(1) == between(MIN, LocalTime.of(0, 0, 0, 1)))

      intercept[DateTimeException](between(MIN, LocalDate.of(2012, 2, 29)))
      intercept[DateTimeException](between(LocalDate.of(2012, 2, 29), LocalDate.of(2012, 3, 1)))
    }
  }
}
