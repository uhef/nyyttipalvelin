package fi.akisaarinen

import scala.actors.Actor

class ItemAverageWeightsSorter extends Algorithm {
  def internalPack(items: List[ContentsItem], capacity: Weight) = {
    sort(capacity, items)
  }

  def sort(capacity: Weight, input: List[ContentsItem]): List[ContentsItem] = {
    val dimImp: List[Double] = calculateDimensionWeightAverages(input).zip(capacity.dimensions).map((avgWithCap) => { avgWithCap._1 / avgWithCap._2 })
    input.sortWith((x, y) => { (x.value / calculateDenominator(x.weight.zip(dimImp))) > (y.value / calculateDenominator(y.weight.zip(dimImp))) })
  }

  def calculateDimensionWeightAverages(items: List[ContentsItem]) : List[Double] = {
    items match {
      case first :: tail => {
        val first = items.head
        val sumOfWeightDimensions = items.tail.foldLeft(first.weight.map( x => {int2double(x)} ))( (x, y) => { x.zip(y.weight).map( z => { z._1 + int2double(z._2) } ) } )
        sumOfWeightDimensions.map( x => { x / items.size } )
      }
      case Nil => Nil
      case singleItem => singleItem.head.weight.map(int2double(_))
    }
  }

  private def calculateDenominator(weightFactorPairs: List[(Int, Double)]) : Double = {
    weightFactorPairs.map((t) => { int2double(t._1) * t._2 }).sum
  }
}