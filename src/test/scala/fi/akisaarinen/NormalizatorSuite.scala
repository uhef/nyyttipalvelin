package fi.akisaarinen

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import math.sqrt

class NormalizatorSuite extends FunSuite with ShouldMatchers {
  private def createItem(weight: List[Int], v: Int) : ContentsItem = {
    new ContentsItem("foo", weight, v)
  }

  private def createItem(item: ContentsItem, w: Double, v: Double) : NormalizedContentsItem = {
    new NormalizedContentsItem(item, w, v)
  }

  test("Normalizator should create list of normalized content items") {
    // Normalized content item is a content item with normalized total weight and normalized value
    // Normalization is done over the collection of items
    def maxW = sqrt(81 + 16 + 49)
    val input = List(createItem(List(1, 2, 3), 1), createItem(List(5, 5, 5), 4), createItem(List(9, 4, 7), 6))
    val output = Normalizator.createNormalizedItems(input)

    output should equal (List(createItem(input(0), sqrt(1 + 4 + 9) / maxW , (1.0 / 6.0)), createItem(input(1), sqrt(25 + 25 + 25) / maxW, (4.0 / 6.0)), createItem(input(2), 1.0, 1.0)))
  }
}

class SimplifierSuite extends FunSuite with ShouldMatchers {
  private def createItem(weight: List[Int], v: Int) : ContentsItem = {
    new ContentsItem("foo", weight, v)
  }

  private def createItem(item: ContentsItem, w: Double, v: Double) : NormalizedContentsItem = {
    new NormalizedContentsItem(item, w, v)
  }

  test("Simplifier should create list of simplified content items") {
    // Simplified content item is a content item with total weight and value
    val input = List(createItem(List(1, 2, 3), 1), createItem(List(5, 5, 5), 4), createItem(List(9, 4, 7), 6))
    val output = Simplifier.createSimplifiedItems(input)

    output should equal (List(createItem(input(0), sqrt(1 + 4 + 9), 1.0), createItem(input(1), sqrt(25 + 25 + 25), 4.0), createItem(input(2), sqrt(81 + 16 + 49), 6.0)))
  }

  test("Simplifier should create lists of simplified content items from two lists of items") {
    // Simplified content item is a content item with total weight and value
    val input1 = List(createItem(List(1, 2, 3), 1), createItem(List(5, 5, 5), 4))
    val input2 = List(createItem(List(9, 4, 7), 6))
    val output = Simplifier.createSimplifiedItems(input1, input2)

    output._1 should equal (List(createItem(input1(0), sqrt(1 + 4 + 9), 1.0), createItem(input1(1), sqrt(25 + 25 + 25), 4.0)))
    output._2 should equal (List(createItem(input2(0), sqrt(81 + 16 + 49), 6.0)))
  }
}
