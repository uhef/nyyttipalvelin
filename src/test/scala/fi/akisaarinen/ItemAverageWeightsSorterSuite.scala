package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ItemAverageWeightsSorterSuite extends FunSuite with ShouldMatchers {
  private def createItem(weight: List[Int], v: Int) : ContentsItem = {
    new ContentsItem("foo", weight, v)
  }

  private def createItemWithoutValue(weight: List[Int]) : ContentsItem = {
    new ContentsItem("foo", weight, 1)
  }

  test("ItemAverageWeightsSorterSuite should calculate average of item dimension weights") {
    val sorter = new ItemAverageWeightsSorter
    val dimensionWeightAverages = sorter.calculateDimensionWeightAverages(List(createItemWithoutValue(List(2, 5, 3)), createItemWithoutValue(List(2, 2, 2)), createItemWithoutValue(List(8, 8, 1))))
    dimensionWeightAverages should equal (List(4, 5, 2))

    sorter.calculateDimensionWeightAverages(List()) should equal (List())

    sorter.calculateDimensionWeightAverages(List(createItemWithoutValue(List(4, 5, 2)))) should equal (List(4, 5, 2))
  }

}