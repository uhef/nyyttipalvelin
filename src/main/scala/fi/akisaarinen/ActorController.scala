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

class ActorController {
  def filterFittingItems(items: List[ContentsItem], capacity: Weight) : Option[List[ContentsItem]] = {
    items.filter(x => { capacity.fits(x.contentsWeight) }) match {
      case List() => None
      case results => Option(results)
    }
  }

  private def average(x: ContentsItem) = x.value / (x.weight.sum)

  def sortToOptimizedOrder(items: List[ContentsItem], capacity: Weight): List[ContentsItem] = {
    val algorithms: List[(List[ContentsItem]) => List[ContentsItem]] = List(sortToOptimizedOrderImpl)
    val resultsFromAlgorithms: List[List[ContentsItem]] = Nyyttimap.runAlgorithms(items, algorithms, capacity)
    ValueUtils.bestList(resultsFromAlgorithms)
  }

  private def sortToOptimizedOrderImpl(items: List[ContentsItem]) = items.sortWith((x, y) => { average(x) > average(y) })

}


object ValueUtils {

  def calculateListValue(items: List[ContentsItem]) = items.map(_.value).foldLeft(0)(_ + _)

  def bestList(lists: List[List[ContentsItem]]) =
    lists.zip(lists.map(calculateListValue(_))).sortWith( (x,y) => x._2 > y._2 ).head._1
}














