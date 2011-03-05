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

trait Algorithm {
  def name = getClass.getSimpleName
  
  def internalPack(items: List[ContentsItem], capacity: Weight): List[ContentsItem]

  def pack(items: List[ContentsItem], capacity: Weight, resultsProcessor: Actor)

  def iterateUntilFull(capacity: Weight, knapsack: List[ContentsItem], remainingList: List[ContentsItem]): List[ContentsItem] = {
    remainingList match {
      case Nil => knapsack
      case nextItem :: remainingItems => {
        val knapsackPlusNew = nextItem :: knapsack
        val totalWeight = knapsackPlusNew.map(_.contentsWeight).foldLeft(Weight(List(0,0,0)))(_.plus(_))
        if (capacity.fits(totalWeight)) {
          iterateUntilFull(capacity, knapsackPlusNew, remainingItems)
        } else {
          knapsack
        }
      }
    }
  }
}

class ActorController {
  def filterFittingItems(items: List[ContentsItem], capacity: Weight): Option[List[ContentsItem]] = {
    items.filter(x => { capacity.fits(x.contentsWeight) }) match {
      case List() => None
      case results => Option(results)
    }
  }

  def chooseItemsToKnapsack(items: List[ContentsItem], capacity: Weight, timeout: Long): List[ContentsItem] = {
    val weightSorter = new WeightSumSorter
    val capacitySorter = new CapacityDimensionWeightedSorter
    val itemAverageWeightSorter = new ItemAverageWeightsSorter
    val brute = new BruteForceFillerAlgorithm
    val weightToTabu = new TabuAlgorithm(timeout - 10000, (new WeightSumSorter).internalPack(_, capacity))
    val capaToTabu = new TabuAlgorithm(timeout - 10000, (new CapacityDimensionWeightedSorter).internalPack(_, capacity))
    val itemAvgToTabu = new TabuAlgorithm(timeout - 10000, (new ItemAverageWeightsSorter).internalPack(_, capacity))

    val algorithms: List[Algorithm] = List(weightSorter, capacitySorter, itemAverageWeightSorter, brute,
      weightToTabu, capaToTabu, itemAvgToTabu)

    filterFittingItems(items, capacity) match {
      case Some(filteredItems) => {
        val resultsOfAlgorithms: Nyyttimap.ResultsOfAlgorithms = Nyyttimap.runAlgorithms(filteredItems, algorithms, capacity, timeout)
        val tooLargeResults: Nyyttimap.ResultsOfAlgorithms = resultsOfAlgorithms.filterNot(x => { capacity.fits(Weight(WeightUtils.totalWeight(x)))} )
        tooLargeResults match {
          case Nil =>
          case tooLarge: Nyyttimap.ResultsOfAlgorithms => println("Error! Too large results, capacity == " + capacity + ", " + tooLarge.map(WeightUtils.totalWeight(_)))
        }
        val resultsWithoutTooLarge = resultsOfAlgorithms.filterNot(list => tooLargeResults.contains(list))
        val bestResults: List[ContentsItem] = ValueUtils.bestList(resultsWithoutTooLarge)
        println("Capacity: " + capacity)
        println("Total weight: " + WeightUtils.totalWeight(bestResults))
        bestResults
      }
      case None => List()
    }
  }
}


object ValueUtils {
  def calculateListValue(items: List[ContentsItem]) = items.map(_.value).foldLeft(0)(_ + _)
  def maxList(a: (List[ContentsItem], Int), b: (List[ContentsItem], Int)) = if(calculateListValue(a._1) >= calculateListValue(b._1)) a else b

  def bestList(lists: ResultsOfAlgorithms) = lists.zip(lists.map(calculateListValue(_))) match {
    case items: List[(List[ContentsItem], Int)] if items != Nil => items.reduceRight(maxList(_,_))._1
    case _ => Nil
  }
}

object WeightUtils {
  def totalWeight(items: List[ContentsItem]) = {
    items.map( x => { x.weight } ).foldLeft(List(0, 0, 0))( (x, y) => { x.zip(y).map( t => { t._1 + t._2 } ) } )
  }
}













