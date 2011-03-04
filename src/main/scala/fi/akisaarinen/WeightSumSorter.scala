package fi.akisaarinen

/**
 * Created by IntelliJ IDEA.
 * User: tuomjarv
 * Date: 3/4/11
 * Time: 10:16 AM
 * To change this template use File | Settings | File Templates.
 */

class WeightSumSorter {
  private def importance(x: ContentsItem) = x.value / (x.weight.sum)

  def sort(input : List[ContentsItem]) : List[ContentsItem] = {
    input.sortWith((x, y) => { importance(x) > importance(y) })
  }
}