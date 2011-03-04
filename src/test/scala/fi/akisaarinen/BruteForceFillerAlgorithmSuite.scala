package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BruteForceFillerAlgorithmSuite extends FunSuite with ShouldMatchers {
  val controller = new BruteForceFillerAlgorithm
  private def createItem(weight: List[Int], v: Int) : ContentsItem = {
     new ContentsItem("foo", weight, v)
  }

  test("BruteForceFiller should return knapsack if it cannot pimp it") {
    val result = controller.tryToOptimizeKnapsack(List(createItem(List(1,2,3), 3)), List(createItem(List(2,3,4), 5)), Weight(List(2,3,4)))


    // val result = controller.filterFittingItems(List(ContentsItem("foo", List(1, 1, 1), 100)), Weight(List(5, 5, 5)))
    // result.size should equal (1)
    // result should equal (Some(List(ContentsItem("foo", List(1, 1, 1), 100))))
  }

  test("BruteForce filler should find the scarcest dimension") {
    val first = controller.calculateDimensionWhichIsScarcest(List(createItem(List(1, 2, 3), 3), createItem(List(2, 3, 4), 5)), Weight(List(3, 7, 8)))
    first should equal (First)
    val second = controller.calculateDimensionWhichIsScarcest(List(createItem(List(1, 2, 3), 3), createItem(List(2, 3, 4), 5)), Weight(List(9, 6, 43)))
    second should equal (Second)
    val third = controller.calculateDimensionWhichIsScarcest(List(createItem(List(1, 2, 3), 3), createItem(List(2, 3, 4), 5)), Weight(List(34, 33, 21)))
    third should equal (Third)
  }

  test("BruteForce filler should find if no dimension is scarcest") {
     val result = controller.calculateDimensionWhichIsScarcest(List(createItem(List(1,2,3), 3), createItem(List(2,3,4), 5)), Weight(List(3,43,7)))
     result should equal (Multiple)
  }
}