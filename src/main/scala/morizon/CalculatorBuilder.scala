package morizon

class CalculatorBuilder() {

  case class Range(from: Percentage, to: Percentage, margins: Seq[Margin]) {
    val sorted = margins.sortWith((a, b) => a.maxLoanValue < b.maxLoanValue)
    def margin(loanValue: Int) = sorted.find(_.maxLoanValue > loanValue).map(_.value)
  }

  var ranges: Seq[Range] = Seq()

  private def validateRanges() = {
    def check(l: List[Range]): Boolean = l match {
      case Nil => true
      case h::Nil => true
      case h::k::rest => h.to.value <= k.from.value && check(k::rest)
    }
    require(check(rangesSorted), "ranges overlaps")
  }

  private def rangesSorted = ranges.toList.sortWith( (a,b) => (a.from.value < b.from.value) )

  def validateMargins(margins: Seq[Margin]) =
    require(margins.length == margins.map(_.maxLoanValue).toSet.size, "not unique forOwnShareTo")

  def addRange(from: Percentage, to: Percentage, margins: Seq[Margin]) = {
    validateMargins(margins)
    ranges = ranges :+ Range(from, to, margins)
    validateRanges()
    this
  }

  def build() = new LoanMarginCalculator {

    val MarginForFirst12Months = 1.1

    override def margin(month: Int, loanValue: Int, ownShare: Int): Option[BigDecimal] = {
      require(month >= 1, "month must be positive")
      require(loanValue > 0, "loanValue must be positive")
      require(ownShare > 0, "ownShare must be positive")
      require(ownShare < loanValue, "ownShare must be less than loanValue")

      if (month <= 12) {
        Option(MarginForFirst12Months)
      }
      else {
        val ownSharePercentage = ownShare * 100.0 / loanValue
        for {
          range <- rangesSorted.find(range => (ownSharePercentage >= range.from.value && ownSharePercentage < range.to.value))
          margin <- range.margin(loanValue)
        } yield margin
      }
    }
  }
}

case class Percentage(value: Int) {
  require(value >= 0 && value <= 100, "percentage must be between 0 and 100")
}

case class Margin(value: BigDecimal, maxLoanValue: Int) {
  require(value >= 0, "value must be >= 0")
  require(maxLoanValue >=0, "maxLoanValue must be >= 0")
}
