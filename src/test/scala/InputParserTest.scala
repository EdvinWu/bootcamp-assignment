import CardSuit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InputParserTest extends AnyFlatSpec with should.Matchers {

  "it" should "not enough river cards" in {
    val res = InputParser.mapToTable(List("4cKs4h8s7", "Ad4s", "Ac4d"))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldBe "Wrong card amount for river"
  }

  "it" should "no hand cards" in {
    val res = InputParser.mapToTable(List("4cKs4h8s7s"))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldBe "No hands provided"
  }

  "it" should "more than 2 hand cards" in {
    val res = InputParser.mapToTable(List("4cKs4h8s7s", "Ad4s", "Ac4dKd"))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldBe "Wrong card amount for hands"
  }

  "it" should "duplicated cards" in {
    val res = InputParser.mapToTable(List("4cKs4h8s7s", "Ad4s", "Ac4d", "Ad6h"))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldBe "Duplicated cards"
  }

  "it" should "be table" in {
    val res = InputParser.mapToTable(List("4cKs4h8s7s", "Ad4s", "Ac4d", "As9s", "KhKd", "5d6d"))
    res.isSuccess shouldBe true
    res.get shouldBe getTable(4)
  }

  "it" should "be table with t" in {
    val res = InputParser.mapToTable(List("TcKs4h8s7s", "Ad4s", "Ac4d", "As9s", "KhKd", "5d6d"))
    res.isSuccess shouldBe true
    res.get shouldBe getTable(10)
  }

  private def getTable(firstCardVal: Int): Table = {
    Table(river = List(Card(firstCardVal, C), Card(13, S), Card(4, H), Card(8, S), Card(7, S)),
      hands = List[Hand](Hand(Card(14, D), Card(4, S)),
        Hand(Card(14, C), Card(4, D)),
        Hand(Card(14, S), Card(9, S)),
        Hand(Card(13, H), Card(13, D)),
        Hand(Card(5, D), Card(6, D)))
    )
  }
}
