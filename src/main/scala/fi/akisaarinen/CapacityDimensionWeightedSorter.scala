package fi.akisaarinen

import scala.actors.Actor

class CapacityDimensionWeightedSorter extends Algorithm {
  def internalPack(items: List[ContentsItem], capacity: Weight) = {
    sort(capacity, items)
  }

  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor) = {
    val resultWithPossiblyTooMuch = internalPack(items, capacity)
    resultsProcessor ! iterateUntilFull(capacity, Nil, resultWithPossiblyTooMuch)
  }

  private def calculateDenominator(weightFactorPairs: List[(Int, Double)]) : Double = {
    weightFactorPairs.map((t) => { int2double(t._1) * t._2 }).sum
  }

  def sort(capacity: Weight, input: List[ContentsItem]) : List[ContentsItem] = {
    val max = capacity.dimensions.max
    val weightFactors = capacity.dimensions.map(x => { 1.0 - int2double(x) / int2double(max) })
    input.sortWith((x, y) => { (x.value / calculateDenominator(x.weight.zip(weightFactors))) > (y.value / calculateDenominator(y.weight.zip(weightFactors))) })
  }
}