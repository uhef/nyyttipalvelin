package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by IntelliJ IDEA.
 * User: tuomjarv
 * Date: 3/4/11
 * Time: 10:17 AM
 * To change this template use File | Settings | File Templates.
 */

class WeightSumSorterSuite extends FunSuite with ShouldMatchers {
  test("WeightSumSorter should order by value / summed weight") {
    val sorter = new WeightSumSorter
    val result = sorter.sort(List(ContentsItem("foo", List(5, 10, 15), 50), ContentsItem("foo", List(2, 4, 6), 40), ContentsItem("foo", List(1, 4, 6), 574)))
    result should equal (List( ContentsItem("foo", List(1, 4, 6), 574), ContentsItem("foo", List(2, 4, 6), 40), ContentsItem("foo", List(5, 10, 15), 50)))
  }
}