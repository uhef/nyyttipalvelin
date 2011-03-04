package fi.akisaarinen

sealed abstract class Scarcest

case object First extends Scarcest
case object Second extends Scarcest
case object Third extends Scarcest
case object Multiple extends Scarcest

class BruteForceFillerAlgorithm {

  def tryToOptimizeKnapsack(knapsack : List[ContentsItem], leftovers : List[ContentsItem], capacity : Weight) : List[ContentsItem] = {
    println (ValueUtils.calculateListValue(knapsack))
    knapsack
  }

  def sortByContentWeightValue(i : Int): (ContentsItem, ContentsItem) => Boolean = {
    (x, y) => {
      x.value / x.weight(i) < y.value / y.weight(i)
    }
  }

  def sortToScarcestDimension(scarce : Scarcest, knapsack : List[ContentsItem]) = {
    scarce match {
      case First => { knapsack.sortWith(sortByContentWeightValue(0)) }
      case Second => { knapsack.sortWith(sortByContentWeightValue(1)) }
      case Third => { knapsack.sortWith(sortByContentWeightValue(2)) }
      case _ => println ("won't optimize")
    }
  }

  def getCapacityContstraint(i :Int, capacity: Weight, knapsack: scala.List[ContentsItem]): Int = {
    capacity.dimensions(i) - knapsack.map {
      case x => {
        x.contentsWeight.dimensions(i)
      }
    }.foldLeft(0)(_ + _)
  }

  def calculateDimensionWhichIsScarcest(knapsack : List[ContentsItem], capacity : Weight) : Scarcest = {
    var first, second, third = Int.MaxValue
    first = getCapacityContstraint(0, capacity, knapsack)
    if(capacity.dimensions.size > 1)
      second = getCapacityContstraint(1, capacity, knapsack)
    if(capacity.dimensions.size > 2)
      third = getCapacityContstraint(2, capacity, knapsack)
    if(first < second && first < third) return First
    if(second < first &&  second < third) return Second
    if(third < first && third < second) return Third
    Multiple
  }
}