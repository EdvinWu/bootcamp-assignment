import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class OutputParserTest extends AnyFlatSpec with should.Matchers {

  "it" should "equal pairs and flush" in {
    OutputParser.mapToOutput(twoPairsAndFlush()) shouldBe "QdQh=QsQc 3s4s"
  }

  "it" should "many equals" in {
    OutputParser.mapToOutput(manyEquals()) shouldBe "7d8h=5s6c=3s4s=3s4s 3s4s"
  }

  def manyEquals() =
    List(RankedHand(
      hand = Hand(c1 = Card(value = 7, suit = CardSuit.D), c2 = Card(value = 8, suit = CardSuit.H)),
      combination = Combination.PAIR,
      combCardRank = 12, decisionCards = List(11, 9, 4)),
      RankedHand(
        hand = Hand(c1 = Card(value = 5, suit = CardSuit.S), c2 = Card(value = 6, suit = CardSuit.C)),
        combination = Combination.PAIR,
        combCardRank = 12, decisionCards = List(11, 9, 4)),
      RankedHand(
        hand = Hand(c1 = Card(value = 3, suit = CardSuit.S), c2 = Card(value = 4, suit = CardSuit.S)),
        combination = Combination.PAIR,
        combCardRank = 12, decisionCards = List(11, 9, 4)),
      RankedHand(
        hand = Hand(c1 = Card(value = 3, suit = CardSuit.S), c2 = Card(value = 4, suit = CardSuit.S)),
        combination = Combination.PAIR,
        combCardRank = 12, decisionCards = List(11, 9, 4)),
      RankedHand(
        hand = Hand(c1 = Card(value = 3, suit = CardSuit.S), c2 = Card(value = 4, suit = CardSuit.S)),
        combination = Combination.STRAIGHT_FLUSH,
        combCardRank = 0, decisionCards = List(5)),
    )

  def twoPairsAndFlush() =
    List(RankedHand(
      hand = Hand(c1 = Card(value = 12, suit = CardSuit.D), c2 = Card(value = 12, suit = CardSuit.H)),
      combination = Combination.PAIR,
      combCardRank = 12, decisionCards = List(11, 9, 4)),
      RankedHand(
        hand = Hand(c1 = Card(value = 12, suit = CardSuit.S), c2 = Card(value = 12, suit = CardSuit.C)),
        combination = Combination.PAIR,
        combCardRank = 12, decisionCards = List(11, 9, 4)),
      RankedHand(
        hand = Hand(c1 = Card(value = 3, suit = CardSuit.S), c2 = Card(value = 4, suit = CardSuit.S)),
        combination = Combination.FLUSH,
        combCardRank = 0, decisionCards = List(11)),
    )
}
