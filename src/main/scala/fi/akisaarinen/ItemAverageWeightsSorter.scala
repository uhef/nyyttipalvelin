package fi.akisaarinen

class ItemAverageWeightsSorter {
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
}