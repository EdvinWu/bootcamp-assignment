import scala.util.{Failure, Success, Try}

object InputParser {
  def mapToTable(l: List[String]): Try[Table] = {
    if (l.size < 2) Failure(new Error("No hands provided"))
    else if (l.head.length != 10) Failure(new Error("Wrong card amount for river"))
    else if (!l.tail.forall(_.length == 4)) Failure(new Error("Wrong card amount for hands"))
    else if (l.flatMap(_.toCharArray.grouped(2)).distinct.size != l.tail.size * 2 + 5) Failure(new Error("Duplicated cards"))
    else Success(Table(mapToRiver(l.head), mapHands(l.tail)))
  }

  private def mapToRiver(str: String) =
    str.toCharArray.grouped(2).map(a => Card(getCardValue(a(0)), CardSuit.fromChar(a(1)))).toList

  private def mapHands(l: List[String]) =
    l.flatMap(s => mapToRiver(s)).grouped(2).map(c => Hand(c.head, c(1))).toList

  private def getCardValue(ch: Char): Int = {
    ch.toLower match {
      case 'a' => 14
      case 'k' => 13
      case 'q' => 12
      case 'j' => 11
      case c: Char => c.asDigit
    }
  }
}
