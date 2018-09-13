package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.chrono.IsoChronology
import java.time.temporal.ChronoUnit

import utest._

object PeriodTestUtils {

  import Period._
  import ChronoUnit._

  final val pmin = of(Int.MinValue, Int.MinValue, Int.MinValue)
  final val pmin1 = of(-Int.MaxValue, -Int.MaxValue, -Int.MaxValue)
  final val pmax = of(Int.MaxValue, Int.MaxValue, Int.MaxValue)
  final val oneYear = of(1, 0, 0)
  final val oneMonth = of(0, 1, 0)
  final val oneDay = of(0, 0, 1)

  val samples1 = Seq(ZERO, oneYear, oneYear.negated,
      oneMonth, oneMonth.negated, oneDay, oneDay.negated)
}

object PeriodTestTemporalAmount extends TestSuite with TemporalAmountTest {
  import Period._
  import ChronoUnit._
  import PeriodTestUtils._

  val samples: Seq[Period] = samples1 ++ Seq(pmin, pmin1, pmax)

  val units = Seq(YEARS, MONTHS, DAYS)

  val tests = temporalAmountTests
}

object PeriodTest extends TestSuite {

  import Period._
  import ChronoUnit._
  import PeriodTestUtils._

  val samples: Seq[Period] = samples1 ++ Seq(pmin, pmin1, pmax)

  val tests = Tests {    
    'test_get - {
      for (p <- samples) {
        assert(p.getYears.toLong == p.get(YEARS))
        assert(p.getMonths.toLong == p.get(MONTHS))
        assert(p.getDays.toLong == p.get(DAYS))
      }
    }

    'test_getChronology - {
      for (p <- samples) {
        assert(IsoChronology.INSTANCE == p.getChronology)
      }
    }

    'test_isZero - {
      for (p <- samples) {
        if (p == ZERO) assert(p.isZero)
        else assert(p.isZero == false)
      }
    }

    'test_isNegative - {
      for (p <- Seq(ZERO, oneYear, oneMonth, oneDay, pmax))
        assert(p.isNegative == false)
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated, pmin, pmin1))
        assert(p.isNegative)
      for (p <- Seq(of(-1, 1, 1), of(1, -1, 1), of(1, 1, -1)))
        assert(p.isNegative)
    }

    'test_getYears - {
      assert(1 == oneYear.getYears)
      assert(0 == oneMonth.getYears)
      assert(0 == oneDay.getYears)
      assert(Int.MinValue == pmin.getYears)
      assert(Int.MinValue + 1 == pmin1.getYears)
      assert(Int.MaxValue == pmax.getYears)
    }

    'test_getMonths - {
      assert(0 == oneYear.getMonths)
      assert(1 == oneMonth.getMonths)
      assert(0 == oneDay.getMonths)
      assert(Int.MinValue == pmin.getMonths)
      assert(Int.MinValue + 1 == pmin1.getMonths)
      assert(Int.MaxValue == pmax.getMonths)
    }

    'test_getDays - {
      assert(0 == oneYear.getDays)
      assert(0 == oneMonth.getDays)
      assert(1 == oneDay.getDays)
      assert(Int.MinValue == pmin.getDays)
      assert(Int.MinValue + 1 == pmin1.getDays)
      assert(Int.MaxValue == pmax.getDays)
    }

    'test_withYears - {
      for {
        p <- samples
        n <- Seq(Int.MinValue, 0, Int.MaxValue)
      } {
        val p1 = p.withYears(n)
        assert(n == p1.getYears)
        assert(p.getMonths == p1.getMonths)
        assert(p.getDays == p1.getDays)
      }
    }

    'test_withMonths - {
      for {
        p <- samples
        n <- Seq(Int.MinValue, 0, Int.MaxValue)
      } {
        val p1 = p.withMonths(n)
        assert(p.getYears == p1.getYears)
        assert(n == p1.getMonths)
        assert(p.getDays == p1.getDays)
      }
    }

    'test_withDays - {
      for {
        p <- samples
        n <- Seq(Int.MinValue, 0, Int.MaxValue)
      } {
        val p1 = p.withDays(n)
        assert(p.getYears == p1.getYears)
        assert(p.getMonths == p1.getMonths)
        assert(n == p1.getDays)
      }
    }

    'test_plus - {
      for {
        p1 <- samples1 :+ pmin1
        p2 <- if (p1 != pmin1) samples1 :+ pmin1 else samples1
      } {
        val p = p1.plus(p2)
        assert(p1.getYears + p2.getYears == p.getYears)
        assert(p1.getMonths + p2.getMonths == p.getMonths)
        assert(p1.getDays + p2.getDays == p.getDays)
      }

      for (p <- Seq(oneYear, oneMonth, oneDay))
        intercept[ArithmeticException](pmax.plus(p))
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated))
        intercept[ArithmeticException](pmin.plus(p))
      for (p <- samples)
        intercept[DateTimeException](p.plus(Duration.ZERO))
    }

    'test_plusYears - {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 1, -1, 0, 1, Int.MaxValue - 1)
      } {
        val p1 = p.plusYears(n)
        assert(p.getYears + n == p1.getYears)
        assert(p.getMonths == p1.getMonths)
        assert(p.getDays == p1.getDays)
      }

      intercept[ArithmeticException](oneYear.plusYears(Int.MaxValue))
      intercept[ArithmeticException](oneYear.negated.plusYears(Int.MinValue))
      intercept[ArithmeticException](pmax.plusYears(1))
      intercept[ArithmeticException](pmin.plusYears(-1))
    }

    'test_plusMonths - {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 1, -1, 0, 1, Int.MaxValue - 1)
      } {
        val p1 = p.plusMonths(n)
        assert(p.getYears == p1.getYears)
        assert(p.getMonths + n == p1.getMonths)
        assert(p.getDays == p1.getDays)
      }

      intercept[ArithmeticException](oneMonth.plusMonths(Int.MaxValue))
      intercept[ArithmeticException](oneMonth.negated.plusMonths(Int.MinValue))
      intercept[ArithmeticException](pmax.plusMonths(1))
      intercept[ArithmeticException](pmin.plusMonths(-1))
    }

    'test_plusDays - {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 1, -1, 0, 1, Int.MaxValue - 1)
      } {
        val p1 = p.plusDays(n)
        assert(p.getYears == p1.getYears)
        assert(p.getMonths == p1.getMonths)
        assert(p.getDays + n == p1.getDays)
      }

      intercept[ArithmeticException](oneDay.plusDays(Int.MaxValue))
      intercept[ArithmeticException](oneDay.negated.plusDays(Int.MinValue))
      intercept[ArithmeticException](pmax.plusDays(1))
      intercept[ArithmeticException](pmin.plusDays(-1))
    }

    'test_minus - {
      for {
        p1 <- samples1 :+ pmin1
        p2 <- if (p1.isNegative) samples1 :+ pmin1 else samples1
      } {
        val p = p1.minus(p2)
        assert(p1.getYears - p2.getYears == p.getYears)
        assert(p1.getMonths - p2.getMonths == p.getMonths)
        assert(p1.getDays - p2.getDays == p.getDays)
      }

      for (p <- Seq(oneYear, oneMonth, oneDay))
        intercept[ArithmeticException](pmin.minus(p))
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated))
        intercept[ArithmeticException](pmax.minus(p))
      for (p <- samples)
        intercept[DateTimeException](p.minus(Duration.ZERO))
    }

    'test_minusYears - {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 2, -1, 0, 1, Int.MaxValue)
      } {
        val p1 = p.minusYears(n)
        assert(p.getYears - n == p1.getYears)
        assert(p.getMonths == p1.getMonths)
        assert(p.getDays == p1.getDays)
      }

      intercept[ArithmeticException](oneYear.minusYears(Int.MinValue + 1))
      intercept[ArithmeticException](pmin.minusYears(1))
      intercept[ArithmeticException](pmax.minusYears(-1))
    }

    'test_minusMonths - {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 2, -1, 0, 1, Int.MaxValue)
      } {
        val p1 = p.minusMonths(n)
        assert(p.getYears == p1.getYears)
        assert(p.getMonths - n == p1.getMonths)
        assert(p.getDays == p1.getDays)
      }

      intercept[ArithmeticException](oneMonth.minusMonths(Int.MinValue + 1))
      intercept[ArithmeticException](pmin.minusMonths(1))
      intercept[ArithmeticException](pmax.minusMonths(-1))
    }

    'test_minusDays - {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 2, -1, 0, 1, Int.MaxValue)
      } {
        val p1 = p.minusDays(n)
        assert(p.getYears == p1.getYears)
        assert(p.getMonths == p1.getMonths)
        assert(p.getDays - n == p1.getDays)
      }

      intercept[ArithmeticException](oneDay.minusDays(Int.MinValue + 1))
      intercept[ArithmeticException](pmin.minusDays(1))
      intercept[ArithmeticException](pmax.minusDays(-1))
    }

    'test_multipliedBy - {
      for {
        p <- samples1
        min = if (p.isNegative) Int.MinValue + 1 else Int.MinValue
        n <- Seq(min, -2, 2, Int.MaxValue)
      } {
        val p1 = p.multipliedBy(n)
        assert(p.getYears * n == p1.getYears)
        assert(p.getMonths * n == p1.getMonths)
        assert(p.getDays * n == p1.getDays)
      }
      for (p <- samples) {
        assert(p == p.multipliedBy(1))
        assert(ZERO == p.multipliedBy(0))
      }
      for (p <- samples if p != pmin)
        assert(p.negated == p.multipliedBy(-1))

      intercept[ArithmeticException](pmin.multipliedBy(2))
      intercept[ArithmeticException](pmin.multipliedBy(-1))
      for (p <- Seq(pmin1, pmax)) {
        intercept[ArithmeticException](p.multipliedBy(2))
        intercept[ArithmeticException](p.multipliedBy(-2))
      }
      for (p <- samples1 if p != ZERO) {
        val p2 = p.multipliedBy(2)
        intercept[ArithmeticException](p2.multipliedBy(Int.MaxValue))
        intercept[ArithmeticException](p2.multipliedBy(Int.MinValue))
      }
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated))
        intercept[ArithmeticException](p.multipliedBy(Int.MinValue))
    }

    'test_negated - {
      for (p <- samples if p != pmin) {
        val p1 = p.negated
        assert(-p.getYears == p1.getYears)
        assert(-p.getMonths == p1.getMonths)
        assert(-p.getDays == p1.getDays)
      }

      intercept[ArithmeticException](pmin.negated)
    }

    'test_normalized - {
      val ps = samples1 ++ Seq(of(1, -1, 0), of(-1, 1, 0)) ++
          Seq(of(1, -25, 1), of(-1, 25, -1), of(1, 13, 1), of(-1, -13, -1))
      for (p <- ps) {
        val p1 = p.normalized
        val years = p1.getYears
        val months = p1.getMonths
        assert(Math.abs(months) < 12)
        assert((years > 0 && months < 0) == false)
        assert((years < 0 && months > 0) == false)
        assert(p.getYears * 12 + p.getMonths == years * 12 + months)
      }

      for (p <- Seq(pmin, pmin1, pmax))
        intercept[ArithmeticException](p.normalized)
    }

    'test_toTotalMonths - {
      for (p <- samples)
        assert((p.getYears.toLong * 12 + p.getMonths) == p.toTotalMonths)
    }

    'test_addTo - {
      val ds = Seq(LocalDate.MIN, LocalDate.MAX, LocalDate.of(2011, 2, 28),
          LocalDate.of(2012, 2, 29))
      val ts = Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX)
      val p1 = Period.of(1, 3, 5)
      val p2 = p1.negated

      for (t <- ds ++ ts)
        assert(t == ZERO.addTo(t))

      assert(LocalDate.MAX == Period.of(1999999998, 11, 30).addTo(LocalDate.MIN))
      assert(LocalDate.MIN == Period.of(-1999999998, -11, -30).addTo(LocalDate.MAX))
      assert(LocalDate.of(2012, 2, 29) == Period.of(1, 0, 1).addTo(LocalDate.of(2011, 2, 28)))
      assert(LocalDate.of(2011, 2, 27) == Period.of(-1, 0, -1).addTo(LocalDate.of(2012, 2, 29)))
      assert(LocalDate.of(2013, 2, 28) == oneYear.addTo(LocalDate.of(2012, 2, 29)))
      assert(LocalDate.of(2012, 2, 29) == Period.of(-1, 0, 1).addTo(LocalDate.of(2013, 2, 28)))

      for (p <- Seq(oneYear, oneMonth, oneYear, pmin, pmax))
        intercept[DateTimeException](p.addTo(LocalDate.MAX))
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneYear.negated, pmin, pmax))
        intercept[DateTimeException](p.addTo(LocalDate.MIN))
      for {
        p <- samples if p != ZERO
        t <- ts
      } {
        intercept[DateTimeException](p.addTo(t))
      }
    }

    'test_subtractFrom - {
      val ds = Seq(LocalDate.MIN, LocalDate.MAX, LocalDate.of(2012, 2, 29))
      val ts = Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX)

      for (t <- ds ++ ts)
        assert(t == ZERO.subtractFrom(t))

      assert(LocalDate.MAX == Period.of(-1999999998, -11, -30).subtractFrom(LocalDate.MIN))
      assert(LocalDate.MIN == Period.of(1999999998, 11, 30).subtractFrom(LocalDate.MAX))
      assert(LocalDate.of(2012, 2, 29) == Period.of(-1, 0, -1).subtractFrom(LocalDate.of(2011, 2, 28)))
      assert(LocalDate.of(2011, 2, 27) == Period.of(1, 0, 1).subtractFrom(LocalDate.of(2012, 2, 29)))
      assert(LocalDate.of(2013, 2, 28) == oneYear.negated.subtractFrom(LocalDate.of(2012, 2, 29)))
      assert(LocalDate.of(2012, 2, 29) == Period.of(1, 0, -1).subtractFrom(LocalDate.of(2013, 2, 28)))

      for (p <- Seq(oneYear.negated, oneMonth.negated, oneYear.negated, pmin, pmax))
        intercept[DateTimeException](p.subtractFrom(LocalDate.MAX))
      for (p <- Seq(oneYear, oneMonth, oneYear, pmin, pmax))
        intercept[DateTimeException](p.subtractFrom(LocalDate.MIN))
      for {
        p <- samples if p != ZERO
        t <- ts
      } {
        intercept[DateTimeException](p.subtractFrom(t))
      }
    }

    'test_toString - {
      assert("P0D" == ZERO.toString)
      assert("P-2147483648Y-2147483648M-2147483648D" == pmin.toString)
      assert("P2147483647Y2147483647M2147483647D" == pmax.toString)
      assert("P1Y" == oneYear.toString)
      assert("P-1Y" == oneYear.negated.toString)
      assert("P1M" == oneMonth.toString)
      assert("P-1M" == oneMonth.negated.toString)
      assert("P1D" == oneDay.toString)
      assert("P-1D" == oneDay.negated.toString)
      assert("P2Y-3M" == of(2, -3, 0).toString)
      assert("P-5Y7D" == of(-5, 0, 7).toString)
      assert("P11M-13D" == of(0, 11, -13).toString)
    }
  }
}
