package fi.akisaarinen

import scala.collection.mutable.Queue
import collection.immutable.List
import actors.Actor

sealed abstract class Move(val item: NormalizedContentsItem)
case class MoveOn(override val item: NormalizedContentsItem) extends Move(item)
case class MoveOff(override val item: NormalizedContentsItem) extends Move(item)

trait Logic

object SimpleModifications extends Logic {
  private def calculateTotalWeight(item: NormalizedContentsItem) = item.totalWeight

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

object NormalizedModifications extends Logic {
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
case class InternalTabuParameters(knapsack: List[NormalizedContentsItem], leftovers: List[NormalizedContentsItem], capacity: Weight, alpha: Double, tabuQueue: Queue[Move], resultsProcessor: Actor)

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

  private def createInternalTabuParameters(tabuParams : TabuParameters) : InternalTabuParameters = {
    val normalizedItems = Simplifier.createSimplifiedItems(tabuParams.knapsack, tabuParams.leftovers)
    InternalTabuParameters(normalizedItems._1, normalizedItems._2, tabuParams.capacity, tabuParams.alpha, tabuParams.tabuQueue, tabuParams.resultsProcessor)
  }

  def optimize(_nextParameters: TabuParameters): Unit = {
    var nextParameters: TabuParameters = _nextParameters
    var internalTabuParameters = createInternalTabuParameters(nextParameters)
    while (running && (System.currentTimeMillis < startTime + timeout)) {
      internalTabuParameters = move(internalTabuParameters)
    }
  }

  private def convertNormalizedItemListToItemList(normalizedItems: List[NormalizedContentsItem]) = normalizedItems.map(_.item)

  private def move(param: InternalTabuParameters): InternalTabuParameters = {
    val filler = new TabuFiller(param.knapsack, param.leftovers, param.capacity, param.alpha, SimpleModifications)
    val thisMove: Move = filler.nextMove
    thisMove match {
      case MoveOn(x) => {
        val newKnapsackOfNormalizedItems = x :: param.knapsack
        val newKnapsack = convertNormalizedItemListToItemList(newKnapsackOfNormalizedItems)
        val newValue: Int = ValueUtils.calculateListValue(newKnapsack)
        if (newValue > bestValue && fits(param.capacity, newKnapsack)) {
          bestValue = newValue
          param.resultsProcessor ! ResultMessage(name, newKnapsack)
        }
        val newLeftovers = param.leftovers.filterNot(_ == x)
        val newAlpha = calculateNewAlphaFrom(param.alpha, param.capacity, newKnapsack)
        addToQueue(param.tabuQueue, thisMove)
        InternalTabuParameters(newKnapsackOfNormalizedItems, newLeftovers, param.capacity, newAlpha, param.tabuQueue, param.resultsProcessor)
      }
      case MoveOff(x) => {
        val newKnapsackOfNormalizedItems = param.knapsack.filterNot(_ == x)
        val newKnapsack = convertNormalizedItemListToItemList(newKnapsackOfNormalizedItems)
        val newValue: Int = ValueUtils.calculateListValue(newKnapsack)
        if (newValue > bestValue && fits(param.capacity, newKnapsack)) {
          bestValue = newValue
          param.resultsProcessor ! ResultMessage(name, newKnapsack)
        }
        val newLeftovers = x :: param.leftovers
        val newAlpha = 1.0
        addToQueue(param.tabuQueue, thisMove)
        InternalTabuParameters(newKnapsackOfNormalizedItems, newLeftovers, param.capacity, newAlpha, param.tabuQueue, param.resultsProcessor)
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

class TabuFiller(knapsack: List[NormalizedContentsItem], leftovers: List[NormalizedContentsItem], capacity: Weight, alpha: Double, logic: Logic) {
  val queueMaxSize = 15
  val tabuQueue: Queue[Move] = new Queue()

  private def calculateHeuristic(move: Move) : Double = {
    SimpleModifications.valueModification(move, move.item.item) - (SimpleModifications.weightModification(move, move.item.item, knapsack.map(_.item), capacity) * alpha)
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