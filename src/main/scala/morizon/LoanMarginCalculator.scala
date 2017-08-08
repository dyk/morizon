package morizon

trait LoanMarginCalculator {

  def margin(month: Int, loanValue: Int, ownShare: Int): Option[BigDecimal]

}
