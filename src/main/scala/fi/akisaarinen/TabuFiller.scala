package fi.akisaarinen

import scala.collection.mutable.Queue
import collection.immutable.List
import actors.Actor

sealed abstract class Move(val item: ContentsItem)
case class MoveOn(override val item: ContentsItem) extends Move(item)
case class MoveOff(override val item: ContentsItem) extends Move(item)

object SimpleModifications {
  private def calculateTotalWeight(item: ContentsItem) = scala.math.sqrt(item.weight.map(scala.math.pow(_, 2)).sum)

  def valueModification(move: Move, item: ContentsItem) : Double = {
    move match {
      case MoveOn(x) => x.value
      case MoveOff(x) => x.value * -1.0
    }
  }
  def weightModification(move: Move, item: ContentsItem, knapsack: List[ContentsItem], capacity: Weight) : Double = {
    move match {
      case MoveOn(x) => { if (capacity.fits(Weight(WeightUtils.totalWeight(item :: knapsack)))) { 0 } else calculateTotalWeight(x) }
      case MoveOff(x) => calculateTotalWeight(x) * -1.0
    }
  }
}

object NormalizedModifications {
  def valueModification(move: Move, item: NormalizedContentsItem) : Double = {
    move match {
      case MoveOn(x) => item.value
      case MoveOff(x) => item.value * -1.0
    }
  }
  def weightModification(move: Move, item: NormalizedContentsItem) : Double = {
    move match {
      case MoveOn(x) => item.totalWeight
      case MoveOff(x) => item.totalWeight * -1.0
    }
  }
}

case class TabuParameters(knapsack: List[ContentsItem], leftovers: List[ContentsItem], capacity: Weight, alpha: Double, tabuQueue: Queue[Move], resultsProcessor: Actor)

class TabuAlgorithm(timeout: Long, initialSort: List[ContentsItem] => List[ContentsItem]) extends Algorithm {
  val startTime = System.currentTimeMillis
  val queueMaxSize = 15
  var bestValue: Int = 0
  var running =true

  override def shutDown {
    println("Shutdown " + getClass.getSimpleName)
    running = false
  }

  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor) = {
    val initialSortedItems: List[ContentsItem] = initialSort(items)
    val initialKnapsack = iterateUntilFull(capacity, Nil, initialSortedItems)
    val initialLeftovers = initialSortedItems.filterNot(initialKnapsack.contains(_))
    val initialParameters = TabuParameters(initialKnapsack, initialLeftovers, capacity, 1.0, new Queue[Move](), resultsProcessor)
    optimize(initialParameters)
  }

  def optimize(_nextParameters: TabuParameters): Unit = {
    var nextParameters: TabuParameters = _nextParameters
    while (running && (System.currentTimeMillis < startTime + timeout)) {
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
    SimpleModifications.valueModification(move, move.item) - (SimpleModifications.weightModification(move, move.item, knapsack, capacity) * alpha)
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