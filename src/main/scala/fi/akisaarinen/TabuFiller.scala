package fi.akisaarinen

import scala.collection.mutable.Queue
import collection.immutable.List
import actors.Actor

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

case class TabuParameters(knapsack: List[ContentsItem], leftovers: List[ContentsItem], capacity: Weight, alpha: Double, tabuQueue: Queue[Move], resultsProcessor: Actor)

class TabuAlgorithm(timeout: Long) extends Algorithm {
  val startTime = System.currentTimeMillis
  val queueMaxSize = 15
  var bestValue: Int = 0

  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor) = {
    val initialSortedItems: List[ContentsItem] = (new WeightSumSorter).internalPack(items, capacity)
    val initialKnapsack = iterateUntilFull(capacity, Nil, initialSortedItems)
    val initialLeftovers = initialSortedItems.filterNot(initialKnapsack.contains(_))
    var nextParameters = TabuParameters(initialKnapsack, initialLeftovers, capacity, 1.0, new Queue[Move](), resultsProcessor)
    while (System.currentTimeMillis < startTime + timeout) {
      nextParameters = move(nextParameters)
    }
  }

  private def move(param: TabuParameters): TabuParameters = {
    val filler = new TabuFiller(param.knapsack, param.leftovers, param.capacity, param.alpha)
    val thisMove: Move = filler.nextMove
    thisMove match {
      case MoveOn(x) => {
        val newKnapsack = x :: param.knapsack
        val newValue: Int = ValueUtils.calculateListValue(newKnapsack)
        if (newValue > bestValue && fits(param.capacity, newKnapsack)) {
          bestValue = newValue
          param.resultsProcessor ! ResultMessage(name, newKnapsack)
        }
        val newLeftovers = param.leftovers.filterNot(_ == x)
        val newAlpha = calculateNewAlphaFrom(param.alpha, param.capacity, newKnapsack)
        addToQueue(param.tabuQueue, thisMove)
        TabuParameters(newKnapsack, newLeftovers, param.capacity, newAlpha, param.tabuQueue, param.resultsProcessor)
      }
      case MoveOff(x) => {
        val newKnapsack = param.knapsack.filterNot(_ == x)
        val newValue: Int = ValueUtils.calculateListValue(newKnapsack)
        if (newValue > bestValue && fits(param.capacity, newKnapsack)) {
          bestValue = newValue
          param.resultsProcessor ! ResultMessage(name, newKnapsack)
        }
        val newLeftovers = x :: param.leftovers
        val newAlpha = 1.0
        addToQueue(param.tabuQueue, thisMove)
        TabuParameters(newKnapsack, newLeftovers, param.capacity, newAlpha, param.tabuQueue, param.resultsProcessor)
      }
    }
  }

  private def fits(capacity: Weight, knapsack: List[ContentsItem]): Boolean = {
    capacity.fits(Weight(WeightUtils.totalWeight(knapsack)))
  }

  private def calculateNewAlphaFrom(oldAlpha: Double, capacity: Weight, knapsack: List[ContentsItem]): Double = {
    if (!fits(capacity, knapsack)) {
      oldAlpha * 1.1
    } else {
      1.0
    }
  }

  private def addToQueue(queue: Queue[Move], move: Move): Unit = {
    queue.enqueue(move)
    if (queue.size > queueMaxSize) {
      queue.dequeue
    }
  }

  override def name = super.name + "-queueSize-" + queueMaxSize

  def internalPack(items: List[ContentsItem], capacity: Weight) = null
}

class TabuFiller(knapsack: List[ContentsItem], leftovers: List[ContentsItem], capacity: Weight, alpha: Double) {
  val queueMaxSize = 15
  val tabuQueue: Queue[Move] = new Queue()

  private def calculateHeuristic(move: Move) : Double = {
    move.valueModification(knapsack) - (move.weightModification(knapsack, capacity) * alpha)
  }

  def nextMove() : Move = {
    val moveActions = knapsack.map(MoveOff(_)) ::: leftovers.map(MoveOn(_))
    val nonTabuMoves = moveActions.filterNot(tabuQueue.contains(_))
    val moveHeuristics = nonTabuMoves.map( x => { (x, calculateHeuristic(x)) } )
    val sortedMoves = moveHeuristics.sortWith( (x, y) => { x._2 > y._2 } )
    val selectedMove = sortedMoves.head._1
    addToQueue(selectedMove)
    selectedMove
  }

  private def addToQueue(move: Move): Unit = {
    tabuQueue.enqueue(move)
    if (tabuQueue.size > queueMaxSize) {
      tabuQueue.dequeue
    }
  }
}