package fi.akisaarinen

import scala.actors.Actor

class WeightSumSorter extends Algorithm {
  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor) = {
    resultsProcessor ! sort(items)
  }

  private def importance(x: ContentsItem) = x.value / (x.weight.sum)

  def sort(input : List[ContentsItem]) : List[ContentsItem] = {
    input.sortWith((x, y) => { importance(x) > importance(y) })
  }
}