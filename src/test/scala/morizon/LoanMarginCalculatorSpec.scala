package morizon

import org.scalatest.{FlatSpec, Matchers}

class LoanMarginCalculatorSpec extends FlatSpec with Matchers {

  "CalculatorBuilder" should "reject not uniq maxLoanValue values" in {
    assertThrows[IllegalArgumentException] {
      new CalculatorBuilder()
        .addRange(
          from = Percentage(10),
          to = Percentage(20),
          margins = Seq(
            Margin(value = 3.94, maxLoanValue = 40),
            Margin(value = 1.73, maxLoanValue = 40)))
        .build()
    }
  }

  it should "reject overlapping ranges" in {
    assertThrows[IllegalArgumentException] {
      new CalculatorBuilder()
        .addRange(
          from = Percentage(10),
          to = Percentage(20),
          margins = Seq.empty)
        .addRange(
          from = Percentage(20),
          to = Percentage(50),
          margins = Seq.empty)
        .addRange(
          from = Percentage(40),
          to = Percentage(100),
          margins = Seq.empty)
        .build()
    }
  }

  trait GivenCalculator {
    lazy val calculator = new CalculatorBuilder()
      .addRange(
        from = Percentage(10),
        to = Percentage(20),
        margins = Seq(
          Margin(value = 3.94, maxLoanValue = 40000),
          Margin(value = 2.49, maxLoanValue = 80000),
          Margin(value = 2.17, maxLoanValue = 120000),
          Margin(value = 2.05, maxLoanValue = 200000),
          Margin(value = 1.83, maxLoanValue = 700000),
          Margin(value = 1.73, maxLoanValue = Integer.MAX_VALUE)))
      .addRange(
        from = Percentage(20),
        to = Percentage(30),
        margins = Seq(
          Margin(value = 3.67, maxLoanValue = 40000),
          Margin(value = 2.32, maxLoanValue = 80000),
          Margin(value = 2.00, maxLoanValue = 120000),
          Margin(value = 1.88, maxLoanValue = 200000),
          Margin(value = 1.66, maxLoanValue = 700000),
          Margin(value = 1.55, maxLoanValue = Integer.MAX_VALUE)))
      .addRange(
        from = Percentage(30),
        to = Percentage(50),
        margins = Seq(
          Margin(value = 3.54, maxLoanValue = 40000),
          Margin(value = 2.29, maxLoanValue = 80000),
          Margin(value = 1.97, maxLoanValue = 120000),
          Margin(value = 1.85, maxLoanValue = 200000),
          Margin(value = 1.65, maxLoanValue = 700000),
          Margin(value = 1.55, maxLoanValue = Integer.MAX_VALUE)))
      .addRange(
        from = Percentage(50),
        to = Percentage(100),
        margins = Seq(
          Margin(value = 2.94, maxLoanValue = 40000),
          Margin(value = 2.18, maxLoanValue = 80000),
          Margin(value = 1.91, maxLoanValue = 120000),
          Margin(value = 1.79, maxLoanValue = 200000),
          Margin(value = 1.55, maxLoanValue = 700000),
          Margin(value = 1.45, maxLoanValue = Integer.MAX_VALUE)))
      .build()
  }

  "Calculator" should "calculate margin for first 12 months" in new GivenCalculator {
    calculator.margin(month = 2, loanValue = 100000, ownShare = 55000).get should be(1.1)
  }

  it should "calculate margin for next months" in new GivenCalculator {
    //25% 80000 - 120000 -> 2.00
    calculator.margin(month = 22, loanValue = 100000, ownShare = 25000).get should be(2.00)
  }

  it should "get margin from higher range" in new GivenCalculator {
    //30% 120000 - 200000  -> 1.85
    calculator.margin(month = 21, loanValue = 150000, ownShare = 50000).get should be(1.85)
  }

  "Incomplete calculator" should "not find margin" in {
    val incomplete = new CalculatorBuilder()
        .addRange(
          from = Percentage(10),
          to = Percentage(20),
          margins = Seq(Margin(value = 1.45, maxLoanValue = Integer.MAX_VALUE)))
        .addRange(
          from = Percentage(20),
          to = Percentage(50),
          margins = Seq(Margin(value = 1.46, maxLoanValue = Integer.MAX_VALUE)))
        .addRange(
          from = Percentage(70),
          to = Percentage(100),
          margins = Seq(Margin(value = 1.47, maxLoanValue = Integer.MAX_VALUE)))
        .build()

    //55%
    val loanValue = 1000000
    incomplete.margin(month = 21, loanValue = loanValue, ownShare = (loanValue * 0.55).toInt).isDefined should be (false)
  }


}
