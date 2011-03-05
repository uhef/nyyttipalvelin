package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TabuFillerSuite extends FunSuite with ShouldMatchers {
  private def createContentItem(weight: List[Int], value: Int) : ContentsItem = {
    new ContentsItem("foo", weight, value)
  }

  test("next move should fill gaps in knapsack") {
    val capacity = Weight(List(5, 5, 5))
    val knapsack = List(createContentItem(List(1, 1, 1), 1), createContentItem(List(2, 2, 2), 2))
    val leftovers = List(ContentsItem("moveme", List(2, 2, 2), 2))

    val filler = new TabuFiller(knapsack, leftovers, capacity, 1.0)
    filler.nextMove should equal (MoveOn(ContentsItem("moveme", List(2, 2, 2), 2)))
  }

  test("next move should remove item from overfilled knapsack") {
    val capacity = Weight(List(5, 5, 5))
    val knapsack = List(ContentsItem("removeme", List(3, 3, 3), 3), createContentItem(List(2, 2, 2), 2), createContentItem(List(1, 1, 1), 1))
    val leftovers = List()

    val filler = new TabuFiller(knapsack, leftovers, capacity, 1.0)
    filler.nextMove should equal (MoveOff(ContentsItem("removeme", List(3, 3, 3), 3)))
  }
}