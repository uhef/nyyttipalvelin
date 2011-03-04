package fi.akisaarinen

sealed abstract class Move(item : ContentsItem) {
  def valueModification(knapsack : List[ContentsItem]) : Double
  def weightModification(knapsack : List[ContentsItem], capacity : Weight) : Double
}
case class MoveOn(item: ContentsItem) extends Move(item) {
  def valueModification(knapsack : List[ContentsItem]) : Double = {
    item.value
  }
  def weightModification(knapsack : List[ContentsItem], capacity : Weight) : Double = {
    if (capacity.fits(Weight(WeightUtils.totalWeight(item :: knapsack)))) {
      0
    } else scala.math.sqrt(item.weight.map(scala.math.pow(_, 2)).sum)
  }
}
case class MoveOff(item : ContentsItem) extends Move(item) {
  def valueModification(knapsack : List[ContentsItem]) : Double = {
    item.value * -1.0
  }
  def weightModification(knapsack : List[ContentsItem], capacity : Weight) : Double = {
    scala.math.sqrt(item.weight.map(scala.math.pow(_, 2)).sum) * -1.0
  }
}

class TabuFiller(knapsack: List[ContentsItem], leftovers: List[ContentsItem], capacity: Weight) {
  val alpha = 1.0

  private def calculateHeuristic(move: Move) : Double = {
    move.valueModification(knapsack) - (move.weightModification(knapsack, capacity) * alpha)
  }

  def nextMove() : Move = {
    val moveActions = knapsack.map(MoveOff(_)) ::: leftovers.map(MoveOn(_))
    val moveHeuristics = moveActions.map( x => { (x, calculateHeuristic(x)) } )
    val sortedMoves = moveHeuristics.sortWith( (x, y) => { x._2 > y._2 } )
    sortedMoves.head._1
  }
}