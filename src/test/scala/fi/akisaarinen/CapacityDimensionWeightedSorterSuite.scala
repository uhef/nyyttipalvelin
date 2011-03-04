package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by IntelliJ IDEA.
 * User: tuomjarv
 * Date: 3/4/11
 * Time: 8:55 AM
 * To change this template use File | Settings | File Templates.
 */

class CapacityDimensionWeightedSorterSuite extends FunSuite with ShouldMatchers {
  private def createItem(weight: List[Int], v: Int) : ContentsItem = {
    new ContentsItem("foo", weight, v)
  }

  test("CapacityDimensionWeightedSorter should sort items according to most constrained weight") {
    val sorter = new CapacityDimensionWeightedSorter(Weight(List(10, 5, 2)))
    val sortedList = sorter.sort(List(createItem(List(1, 1, 2), 10), createItem(List(1, 1, 0), 100), createItem(List(3, 2, 1), 10)))
    sortedList should equal (List(createItem(List(1, 1, 0), 100), createItem(List(3, 2, 1), 10), createItem(List(1, 1, 2), 10)))
  }
}