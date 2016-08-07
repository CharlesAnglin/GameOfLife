package gameOfLife

import scala.io.StdIn._

object Game {
  def main(args: Array[String]): Unit = {

    def selectBoard : Tuple2[Int, Int] = {
      var size = readLine("Select board size, input in the form: int, int")
      try {
        val sizeSplit = size.split(", ")
        val tup = (sizeSplit(0).trim.toInt, sizeSplit(1).trim.toInt)
        tup
      } catch {
        case _ : Exception =>
          println("unrecognised input")
          selectBoard
      }
    }

    val boardSize = selectBoard
    val board = new Board(boardSize._1, boardSize._2)
    val printer = new Printer
    board.seed

    for(a <- 0 to 30){
      Thread.sleep(2000)
      printer.print(board)
      board.nextGeneration
    }


  }
}
