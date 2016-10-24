package gameOfLife

import scala.collection.mutable.ListBuffer

class Printer {

//  println("\u220E")

  def print(board : Board) = {
    val boardSize = board.boardSize
    var printBoard = ListBuffer[String]()

    for(y <- 0 to boardSize._2){
      var row = ""
      for(x <- 0 to boardSize._1){
        if(board.getCell(x,y).getCurrentState){
          row += "O"
        } else {
          row += " "
        }
      }
      printBoard += row
    }

    println()
    println("----------------")
    println()
    printBoard.foreach(println(_))
  }


}
