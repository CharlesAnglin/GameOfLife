package gameOfLife

class Cell(val coord : Tuple2[Int, Int]) {

  private var currentState = false
  private var nextState = false

  def getCurrentState = currentState

  def setNextState(bool : Boolean) = nextState = bool

  def nextGeneration = currentState = nextState
}
