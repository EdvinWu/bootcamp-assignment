import Combination.Comb

import scala.math.Ordering.Implicits.seqOrdering

object Evaluator {
  def rankHand(river: List[Card], hand: Hand): RankedHand = {
    val fullHand = river.appended[Card](hand.c1).appended[Card](hand.c2).sorted
    val group =
      fullHand.groupBy(rankMatches(_, fullHand)).view.mapValues {
        _.sorted
      }
    val (fourOfKind, threeOfKind, twoOfKind) = (group.get(4).orNull, group.get(3).orNull, group.get(2).orNull)
    val straightFlush = getStraight(getFlush(fullHand))
    val straight = getStraight(fullHand)
    val flush = getFlush(fullHand)

    if (fourOfKind != null)
      RankedHand(hand, Combination.FOUR_OF_A_KIND, fourOfKind.head.value,   highestNCard(fullHand, fourOfKind, 1))
    else if (straightFlush != null)
      RankedHand(hand, Combination.STRAIGHT_FLUSH, highest(straightFlush, List.empty).head, List.empty)
    else if (threeOfKind != null && twoOfKind != null)
      RankedHand(hand, Combination.FULL_HOUSE, threeOfKind.head.value, List(twoOfKind.head.value))
    else if (flush != null)
      RankedHand(hand, Combination.FLUSH, highestNCard(flush, List.empty, 1).head, List.empty)
    else if (straight != null)
      RankedHand(hand, Combination.STRAIGHT, highest(straight, List.empty).head, List.empty)
    else if (threeOfKind != null)
      RankedHand(hand, Combination.THREE_OF_A_KIND, threeOfKind.head.value, highestNCard(fullHand, threeOfKind, 2))
    else if (twoOfKind != null && group(2).size >= 4)
      RankedHand(hand, Combination.TWO_PAIR, twoOfKind.max.value, highestNCard(fullHand, twoOfKind, 1))
    else if (twoOfKind != null)
      RankedHand(hand, Combination.PAIR, twoOfKind.head.value, highestNCard(fullHand, twoOfKind, 3))
    else {
      val highestCards = highestNCard(fullHand, List.empty, 5)
      RankedHand(hand, Combination.HIGH_CARD, highestCards.head, highestCards.slice(1, highestCards.length))
    }
  }

  private def getFlush(l: List[Card]) = {
    l.groupBy(_.suit).values.find(_.size >= 5).orNull
  }

  private def getStraight(l: List[Card]): List[Int] = {
    if (l == null) {
      return null
    }
    val lowRankAce = l.find(_.value == 14).map(_.suit).map(Card(1, _))
    val resList = l.++(lowRankAce).sorted.map(_.value).distinct
      .sliding(5).filter(l => l.max - l.min == 4).map(_.toList).toList
    if (resList.isEmpty) {
      return null
    }
    resList.max
  }

  private def rankMatches(card: Card, l: List[Card]) =
    l.count(_.value == card.value)

  private def highestNCard(l: List[Card], not: List[Card], n: Int) =
    l.toBuffer.--=(not).sorted.toVector.takeRight(n).map(_.value).toList.reverse

  private def highest(l: List[Int], not: List[Int]) =
    l.toBuffer.--=(not).sorted.toVector.takeRight(1).toList
}

case class RankedHand(hand: Hand, combination: Comb, combCardRank: Int, decisionCards: List[Int]) extends Ordered[RankedHand] {
  override def compare(that: RankedHand): Int = {
    if (that == null) return 1
    if (this.combination == that.combination) {
      if (this.combCardRank == that.combCardRank) {
        for ((v, i) <- this.decisionCards.sorted.zipWithIndex) {
          val thatV = that.decisionCards.sorted.apply(i)
          if (v != thatV) {
            return v.compareTo(thatV)
          }
        }
      }
      this.combCardRank.compareTo(that.combCardRank)
    }
    this.combination.compareTo(that.combination)
  }
}


object Combination extends Enumeration {
  type Comb = Int
  val STRAIGHT_FLUSH = 8
  val FOUR_OF_A_KIND = 7
  val FULL_HOUSE = 6
  val FLUSH = 5
  val STRAIGHT = 4
  val THREE_OF_A_KIND = 3
  val TWO_PAIR = 2
  val PAIR = 1
  val HIGH_CARD = 0
}
