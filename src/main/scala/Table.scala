import CardSuit.Suit

case class Table(river: List[Card], hands: List[Hand]) {
  def sortHands(): List[RankedHand] = {
    hands.map(Evaluator.rankHand(river, _))
  }
}

case class Card(value: Int, suit: Suit) extends Ordered[Card] {
  override def compare(that: Card): Int = this.value.compare(that.value)
}

case class Hand(c1: Card, c2: Card)

object CardSuit extends Enumeration {
  type Suit = Value
  val D = Value("d")
  val S = Value("s")
  val H = Value("h")
  val C = Value("c")

  override def toString(): String =
    CardSuit match {
      case D => "d"
      case S => "s"
      case H => "h"
      case C => "c"
      case _ => ""
    }

  def fromChar(c: Char): Suit = {
    c.toLower match {
      case 'd' => D
      case 's' => S
      case 'h' => H
      case 'c' => C
    }
  }
}