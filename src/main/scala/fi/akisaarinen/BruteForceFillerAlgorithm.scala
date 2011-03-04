package fi.akisaarinen

import scala.actors.Actor

sealed abstract class Scarcest { def getIndex : Int }
case object First extends Scarcest { val getIndex = 0 }
case object Second extends Scarcest { val getIndex = 1 }
case object Third extends Scarcest { val getIndex = 2 }

class BruteForceFillerAlgorithm extends Algorithm {

  def internalPack(items: List[ContentsItem], capacity: Weight) = items

  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor) = {
    val sorter = new ItemAverageWeightsSorter
    val initialKnapsack = iterateUntilFull(capacity, Nil, sorter.internalPack(items, capacity))
    optimizeKnapsack(initialKnapsack, items.filterNot(initialKnapsack.contains(_)), capacity, resultsProcessor)
  }

  def optimizeKnapsack(knapsack : List[ContentsItem], leftovers : List[ContentsItem], capacity : Weight, resultsProcessor: Actor) : List[ContentsItem] = {
    if (leftovers.isEmpty || knapsack.isEmpty) {
      resultsProcessor ! ResultMessage(name + "empty-leftovers", knapsack)
      return knapsack
    }

    val scarcestDimension = calculateDimensionWhichIsScarcest(knapsack, capacity)
    val sortedKnapsack = sortToScarcestDimension(scarcestDimension, knapsack)
    val sortedLeftovers =  sortToScarcestDimension(scarcestDimension, leftovers).reverse

    val candidateToAdd: ContentsItem = sortedLeftovers.head
    val splittedKnapsack = splitKnapsackForItem(Nil, sortedKnapsack, candidateToAdd, scarcestDimension)

    val remainersInKnapsack: List[ContentsItem] = splittedKnapsack._2
    if (weightAvailable(remainersInKnapsack :+ candidateToAdd, capacity).forall(_ >= 0)) {
      // value has to be increased
      val itemsToBeReplaced: List[ContentsItem] = splittedKnapsack._1
      if (ValueUtils.calculateListValue(itemsToBeReplaced) < candidateToAdd.value) {
        val newKnapsack = remainersInKnapsack :+ candidateToAdd
        resultsProcessor ! ResultMessage(name + "-optimised", newKnapsack)
        return optimizeKnapsack(newKnapsack, sortedLeftovers.tail ::: itemsToBeReplaced, capacity, resultsProcessor)
      }
    }
    optimizeKnapsack(sortedKnapsack, sortedLeftovers.tail, capacity, resultsProcessor)
  }

  def weightAvailable(items: List[ContentsItem], capacity: Weight) : List[Int] = {
    val totalWeight = WeightUtils.totalWeight(items)
    capacity.dimensions.zip(totalWeight).map( t => { t._1 - t._2 } )
  }

  // Returns tuple of split knapsack: first list are the items to be removed from knapsack to fit replacement
  // on the scarcest dimension. Second list are the items that are left in knapsack.
  def splitKnapsackForItem(head: List[ContentsItem], rest: List[ContentsItem], replacement: ContentsItem, scarcest: Scarcest) : (List[ContentsItem], List[ContentsItem]) = {
    if (rest.isEmpty) return (head, rest)
    val sum = calculateSumOverDimension(head, scarcest.getIndex)

    if (sum >= replacement.weight(scarcest.getIndex)) (head, rest)
    else splitKnapsackForItem(head :+ rest.head, rest.tail, replacement, scarcest)
  }

  def sortByContentWeightValue(i : Int): (ContentsItem, ContentsItem) => Boolean = {
    (x, y) => {
      x.value / x.weight(i) < y.value / y.weight(i)
    }
  }

  def sortToScarcestDimension(scarce : Scarcest, knapsack : List[ContentsItem]) : List[ContentsItem] = {
    scarce match {
      case First => { knapsack.sortWith(sortByContentWeightValue(0)) }
      case Second => { knapsack.sortWith(sortByContentWeightValue(1)) }
      case Third => { knapsack.sortWith(sortByContentWeightValue(2)) }
      case _ => knapsack
    }
  }

  def calculateSumOverDimension(knapsack: List[ContentsItem], i: Int) = knapsack.map { case x => { x.contentsWeight.dimensions(i) }} .foldLeft(0)(_ + _)

  def getCapacityConstraint(i :Int, capacity: Weight, knapsack: scala.List[ContentsItem]): Int = {
    capacity.dimensions(i) - calculateSumOverDimension(knapsack, i)
  }

  def calculateDimensionWhichIsScarcest(knapsack : List[ContentsItem], capacity : Weight) : Scarcest = {
    var first, second, third = Int.MaxValue
    first = getCapacityConstraint(0, capacity, knapsack)
    if(capacity.dimensions.size > 1)
      second = getCapacityConstraint(1, capacity, knapsack)
    if(capacity.dimensions.size > 2)
      third = getCapacityConstraint(2, capacity, knapsack)
    if(first < second && first < third) return First
    if(second < first &&  second < third) return Second
    if(third < first && third < second) return Third
    Third
  }
}