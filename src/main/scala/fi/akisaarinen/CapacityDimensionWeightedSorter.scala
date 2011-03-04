package fi.akisaarinen

/**
 * Created by IntelliJ IDEA.
 * User: tuomjarv
 * Date: 3/4/11
 * Time: 9:00 AM
 * To change this template use File | Settings | File Templates.
 */

class CapacityDimensionWeightedSorter(capacity: Weight) {
  val max = capacity.dimensions.max
  val weightFactors = capacity.dimensions.map(x => { 1.0 - int2double(x) / int2double(max) })

  def sort(input: List[ContentsItem]) : List[ContentsItem] = {
    input.sortWith((x, y) => { (x.value / x.weight.zip(weightFactors).map { case (t1, t2) => { t1 * t2 } } .sum) > (y.value / y.weight.zip(weightFactors).map { case (t1, t2) => { t1 * t2 } } .sum) })
  }
}