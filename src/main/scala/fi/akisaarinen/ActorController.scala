package fi.akisaarinen

import org.scalatra._
import java.net.URL
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonDSL
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonParser
import net.liftweb.json.JsonParser._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.json.JsonAST.JValue
import collection.immutable.List

import fi.akisaarinen.Nyyttimap._


class ActorController {
  def filterFittingItems(items: List[ContentsItem], capacity: Weight) : Option[List[ContentsItem]] = {
    items.filter(x => { capacity.fits(x.contentsWeight) }) match {
      case List() => None
      case results => Option(results)
    }
  }

  def chooseItemsToKnapsack(items: List[ContentsItem], capacity: Weight): List[ContentsItem] = {
    val weightSorter = new WeightSumSorter
    val capacitySorter = new CapacityDimensionWeightedSorter(capacity)
    val algorithms: List[Algorithm] = List(weightSorter.sort, capacitySorter.sort)
    val resultsFromAlgorithms: AlgorithmResults = Nyyttimap.runAlgorithms(items, algorithms, capacity)

    ValueUtils.bestList(resultsFromAlgorithms)
  }
}


object ValueUtils {

  def calculateListValue(items: List[ContentsItem]) = items.map(_.value).foldLeft(0)(_ + _)

  def maxList(a: (List[ContentsItem], Int), b: (List[ContentsItem], Int)) = if(calculateListValue(a._1) >= calculateListValue(b._1)) a else b

  def bestList(lists: AlgorithmResults) = lists.zip(lists.map(calculateListValue(_))).reduceRight(maxList(_,_))._1
}














