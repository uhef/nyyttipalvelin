package fi.akisaarinen

import scala.actors.Actor

class WeightSumSorter extends Algorithm {
  def internalPack(items: List[ContentsItem], capacity: Weight) = {
    sort(items)
  }

  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor) = {
    val resultWithPossiblyTooMuch = internalPack(items, capacity)
    resultsProcessor ! iterateUntilFull(capacity, Nil, resultWithPossiblyTooMuch)
  }

  private def importance(x: ContentsItem) = x.value / (x.weight.sum)

  def sort(input : List[ContentsItem]) : List[ContentsItem] = {
    input.sortWith((x, y) => { importance(x) > importance(y) })
  }
}