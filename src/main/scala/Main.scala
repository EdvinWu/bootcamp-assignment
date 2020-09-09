
import io.StdIn.readLine

object Main extends App {
  LazyList.
    continually(readLine)
    .takeWhile(_ != null)
    .map(s => InputParser.mapToTable(s.split(" ").toList))
    .foreach(table =>
      if (table.isFailure) {
        println(table.failed.get.getMessage)
      } else {
        println(OutputParser.mapToOutput(table.get.sortHands()))
      }
    )
}





