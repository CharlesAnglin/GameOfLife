package gameOfLife

object Game {
  def main(args: Array[String]): Unit = {

    val board = new Board((5,5))
    val printer = new Printer

    for(a <- 0 to 30){
      Thread.sleep(2000)
      printer.print(board)
      board.nextGeneration
    }


  }
}
