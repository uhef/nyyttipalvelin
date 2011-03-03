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

  test("ActorController should keep item that fits into knapsack") {
    val controller = new ActorController
    val result = controller.filterFittingItems(List(ContentsItem("foo", List(1, 1, 1), 100)), Weight(List(5, 5, 5)))
    result.size should equal (1)
    result(0) should equal (ContentsItem("foo", List(1, 1, 1), 100))
  }

  test("ActorController should remove items that don't fit into knapsack") {
    val controller = new ActorController
    val result = controller.filterFittingItems(List(ContentsItem("foo", List(8, 5, 2), 100), ContentsItem("foo", List(2, 9, 0), 100)), Weight(List(5, 5, 5)))
    result.size should equal (0)
  }

}