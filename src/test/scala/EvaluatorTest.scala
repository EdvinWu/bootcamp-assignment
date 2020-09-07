import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluatorTest extends AnyFlatSpec with should.Matchers {

  "it" should "be straight" in {
    val list = List(Card(11, CardSuit.H), Card(10, CardSuit.S), Card(2, CardSuit.H), Card(9, CardSuit.S), Card(6, CardSuit.C))
    val hand = Hand(Card(13, CardSuit.H), Card(12, CardSuit.H))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.STRAIGHT)
    resHand.decisionCards.shouldBe(List.empty)
    resHand.combCardRank.shouldBe(13)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be two pairs" in {
    val list = List(Card(11, CardSuit.H), Card(10, CardSuit.S), Card(2, CardSuit.H), Card(13, CardSuit.S), Card(6, CardSuit.C))
    val hand = Hand(Card(11, CardSuit.D), Card(10, CardSuit.C))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.TWO_PAIR)
    resHand.decisionCards.shouldBe(List(13))
    resHand.combCardRank.shouldBe(11)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be full house" in {
    val list = List(Card(11, CardSuit.H), Card(10, CardSuit.S), Card(10, CardSuit.H), Card(9, CardSuit.S), Card(6, CardSuit.C))
    val hand = Hand(Card(11, CardSuit.D), Card(10, CardSuit.C))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.FULL_HOUSE)
    resHand.decisionCards.shouldBe(List(11))
    resHand.combCardRank.shouldBe(10)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be four of a kind" in {
    val list = List(Card(11, CardSuit.H), Card(10, CardSuit.S), Card(10, CardSuit.H), Card(10, CardSuit.D), Card(6, CardSuit.C))
    val hand = Hand(Card(11, CardSuit.D), Card(10, CardSuit.C))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.FOUR_OF_A_KIND)
    resHand.decisionCards.shouldBe(List(11))
    resHand.combCardRank.shouldBe(10)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be three of a kind" in {
    val list = List(Card(11, CardSuit.H), Card(10, CardSuit.S), Card(10, CardSuit.H), Card(4, CardSuit.D), Card(6, CardSuit.C))
    val hand = Hand(Card(12, CardSuit.D), Card(10, CardSuit.C))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.THREE_OF_A_KIND)
    resHand.decisionCards.shouldBe(List(12, 11))
    resHand.combCardRank.shouldBe(10)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be flush" in {
    val list = List(Card(11, CardSuit.H), Card(14, CardSuit.D), Card(10, CardSuit.H), Card(4, CardSuit.D), Card(6, CardSuit.D))
    val hand = Hand(Card(2, CardSuit.D), Card(5, CardSuit.D))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.FLUSH)
    resHand.decisionCards.shouldBe(List.empty)
    resHand.combCardRank.shouldBe(14)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be straight flush" in {
    val list = List(Card(14, CardSuit.H), Card(14, CardSuit.D), Card(10, CardSuit.H), Card(4, CardSuit.D), Card(3, CardSuit.D))
    val hand = Hand(Card(2, CardSuit.D), Card(5, CardSuit.D))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.STRAIGHT_FLUSH)
    resHand.decisionCards.shouldBe(List.empty)
    resHand.combCardRank.shouldBe(5)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be pair" in {
    val list = List(Card(14, CardSuit.H), Card(14, CardSuit.D), Card(10, CardSuit.H), Card(4, CardSuit.D), Card(3, CardSuit.D))
    val hand = Hand(Card(7, CardSuit.D), Card(5, CardSuit.C))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.PAIR)
    resHand.decisionCards.shouldBe(List(10, 7, 5))
    resHand.combCardRank.shouldBe(14)
    resHand.hand.shouldBe(hand)
  }

  "it" should "be higher card" in {
    val list = List(Card(14, CardSuit.H), Card(9, CardSuit.D), Card(10, CardSuit.H), Card(4, CardSuit.D), Card(3, CardSuit.D))
    val hand = Hand(Card(7, CardSuit.D), Card(5, CardSuit.C))
    val resHand = Evaluator.rankHand(list, hand)
    resHand.combination.shouldBe(Combination.HIGH_CARD)
    resHand.decisionCards.shouldBe(List(10, 9, 7, 5))
    resHand.combCardRank.shouldBe(14)
    resHand.hand.shouldBe(hand)
  }
}