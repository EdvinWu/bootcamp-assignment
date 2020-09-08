import scala.annotation.tailrec

object OutputParser {
  def mapToOutput(hands: List[RankedHand]): String = {
    val builder = new StringBuilder
    doMapToOutput(hands, builder, null)
  }

  @tailrec
  private def doMapToOutput(hands: List[RankedHand], builder: StringBuilder, prev: RankedHand): String = {
    hands.sorted match {
      case Nil => ""
      case head :: Nil =>
        append(prev, builder, head)
        builder.toString
      case head :: tail =>
        append(prev, builder, head)
        doMapToOutput(tail, builder, head)
    }
  }

  private def append(prev: RankedHand, builder: StringBuilder, head: RankedHand): Unit = {
    if (prev == null) ()
    else if (head.compareTo(prev) == 0) builder.append("=")
    else builder.append(" ")
    builder.append(getHandString(head.hand))
  }

  private def getHandString(hand: Hand): String = {
    getCardChar(hand.c1.value)
      .concat(hand.c1.suit.toString)
      .concat(getCardChar(hand.c2.value)
        .concat(hand.c2.suit.toString))
  }

  private def getCardChar(intVal: Int): String = {
    intVal match {
      case 14 => "A"
      case 13 => "K"
      case 12 => "Q"
      case 11 => "J"
      case 10 => "T"
      case i: Int => i.toString
    }
  }
}
