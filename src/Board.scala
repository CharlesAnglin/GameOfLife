package gameOfLife

import scala.collection.mutable.ListBuffer

class Board(val boardSize : (Int, Int)) {    //, seed : List[Boolean]

  private var board = new ListBuffer[ListBuffer[Cell]]()

  //Instantiates the cells on the board.
  for(a <- 0 to boardSize._1){
    var column = new ListBuffer[Cell]()
    for(b <- 0 to boardSize._2){
      column += new Cell((a,b))
    }
    board += column
  }

//  temporary seed
  val r = scala.util.Random
  for(a <- 0 to 5) {
    var x = r.nextInt(boardSize._1)
    var y =r.nextInt(boardSize._2)
    board(x)(y).setNextState(true)
    board(x)(y).nextGeneration
  }

  def getBoard = board

  def getCell(coord: Tuple2[Int, Int]) = board(coord._1)(coord._2)

  def getAdjacentCells(coord: Tuple2[Int, Int]) = {  //returns ListBuffer of coords for cells.
    var adjacentCells = ListBuffer[Tuple2[Int, Int]]()
    for(a <- coord._1 - 1 to coord._1 + 1){
      for(b <- coord._2 - 1 to coord._2 + 1){
        adjacentCells += a -> b  //syntax (a,b) doesn't work?
      }
    }

    def filterCells(cells : ListBuffer[Tuple2[Int, Int]]) = {
      adjacentCells = cells.filter(_!=(coord._1,coord._2))  //remove self cell
      var newAdjacentCells = new ListBuffer[Tuple2[Int, Int]]()
      adjacentCells.foreach(newAdjacentCells += cellMatch(_))
      newAdjacentCells
    }

    def cellMatch(cell : Tuple2[Int, Int]) : Tuple2[Int, Int] = {
      (cell._1, cell._2) match {
        case (-1, -1) => (boardSize._1, boardSize._2)
        case (-1, b) if b == boardSize._2 + 1 => (boardSize._1, 0)
        case (a, -1) if a == boardSize._1 + 1 => (0, boardSize._2)
        case (a, b) if a == boardSize._1 + 1 && b == boardSize._2 + 1 => (0, 0)
        case (-1, b) => (boardSize._1, b)
        case (a, -1) => (a, boardSize._1)
        case (a, b) if a == boardSize._1 + 1 => (0, b)
        case (a, b) if b == boardSize._2 + 1 => (a, 0)
        case (a, b) => (a, b)
      }
    }

    filterCells(adjacentCells)
  }

  //Map is a torrus
  def countCells(coord : Tuple2[Int, Int]) = {
    var cellCount = 0

    var adjacentCells = getAdjacentCells(coord)
    for(a <- adjacentCells){
      if(getCell(a).getCurrentState){
        cellCount += 1
      }
    }

    cellCount
  }

  def nextGeneration = {
    for(x <- 0 to boardSize._1){
      for(y <- 0 to boardSize._2){
        val cell = getCell((x,y))
        countCells((x,y)) match {
          case a if a < 2 => cell.setNextState(false)
          case a if a == 2 => cell.setNextState(cell.getCurrentState)
          case a if a == 3 => cell.setNextState(!cell.getCurrentState)
          case _ => cell.setNextState(false)
        }
      }
    }
    for(x <- 0 to boardSize._1) {
      for (y <- 0 to boardSize._2) {
        getCell((x, y)).nextGeneration
      }
    }
  }


}
