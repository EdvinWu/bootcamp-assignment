
import io.StdIn.readLine

object Main extends App {
  println("please enter cards")
  val text: Array[String] = Iterator.
    continually(readLine)
    .takeWhile(_ != null)
    .mkString("\n")
    .split("\n")

  text.foreach(x => {
    val table = InputParser.mapToTable(x.split(" ").toList)
    if (table.isFailure) {
      println(table.failed.get.getMessage)
      sys.exit(1)
    }
    println(OutputParser.mapToOutput(table.get.sortHands()))
  })
}





