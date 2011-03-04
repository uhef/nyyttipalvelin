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

  def sortToScarcestDimension(scarce : Scarcest, knapsack : List[ContentsItem]) = {
    scarce match {
      case First => println( "first"  )
      case _ => println ("lol")
    }
  }

  def calculateDimensionWhichIsScarcest(knapsack : List[ContentsItem], capacity : Weight) : Scarcest = {
    var first, second, third = Int.MaxValue
    first = capacity.dimensions(0) - knapsack.map{ case x => { x.contentsWeight.dimensions(0) } }.foldLeft(0)(_ + _)
    if(capacity.dimensions.size > 1)
      second = capacity.dimensions(1) - knapsack.map{ case x => { x.contentsWeight.dimensions(1) } }.foldLeft(0)(_ + _)
    if(capacity.dimensions.size > 2)
      third = capacity.dimensions(2) - knapsack.map{ case x => { x.contentsWeight.dimensions(2) } }.foldLeft(0)(_ + _)
    if(first < second && first < third) return First
    if(second < first &&  second < third) return Second
    if(third < first && third < second) return Third
    Multiple
  }
}