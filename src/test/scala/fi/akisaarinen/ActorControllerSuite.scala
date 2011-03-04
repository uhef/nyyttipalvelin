package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by IntelliJ IDEA.
 * User: tuomjarv
 * Date: 3/3/11
 * Time: 10:25 PM
 * To change this template use File | Settings | File Templates.
 */

class ActorControllerSuite extends FunSuite with ShouldMatchers {
  val controller = new ActorController
  test("ActorController should keep item that fits into knapsack") {
    val result = controller.filterFittingItems(List(ContentsItem("foo", List(1, 1, 1), 100)), Weight(List(5, 5, 5)))
    result.size should equal (1)
    result should equal (Some(List(ContentsItem("foo", List(1, 1, 1), 100))))
  }

  test("ActorController should remove items that don't fit into knapsack") {
    val result = controller.filterFittingItems(List(ContentsItem("foo", List(8, 5, 2), 100), ContentsItem("foo", List(2, 9, 0), 100)), Weight(List(5, 5, 5)))
    result.size should equal (0)
    result should equal (None)
  }

  test("ActorController should order by value / average weight") {
    val result = controller.chooseItemsToKnapsack(List(ContentsItem("foo", List(5, 10, 15), 50), ContentsItem("foo",
      List(2, 4, 6), 40), ContentsItem("foo", List(1, 4, 6), 574)), Weight(List(10000)))
    result should equal (List( ContentsItem("foo", List(1, 4, 6), 574), ContentsItem("foo", List(2, 4, 6), 40), ContentsItem("foo", List(5, 10, 15), 50)).reverse)
  }

}