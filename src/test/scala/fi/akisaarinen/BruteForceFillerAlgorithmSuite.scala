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

   test("BruteForce filler should order based on the scarcest dimension") {
     val result = controller.sortToScarcestDimension(First,List(createItem(List(2,3,4), 3), createItem(List(3,2,3), 1)))
     result should equal (List(createItem(List(3,2,3), 1), createItem(List(2,3,4), 3)))
     val result2 = controller.sortToScarcestDimension(Second, List(createItem(List(2,5,4), 5), createItem(List(1,4,3), 3)))
     result2 should equal (List(createItem(List(1,4,3), 3), createItem(List(2,5,4), 5)))
     val result3 = controller.sortToScarcestDimension(Third, List(createItem(List(1,4,3), 3), createItem(List(2,3,8), 5)))
     result3 should equal (List(createItem(List(2,3,8), 5), createItem(List(1,4,3), 3)))
   }
}