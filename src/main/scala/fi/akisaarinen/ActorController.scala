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
    val filtered = items.filter(x => { capacity.fits(x.contentsWeight) } )
    filtered match {
      case List() => None
      case _ => Option(filtered)
    }
  }

  private def average(x: ContentsItem) = x.value / (x.weight.sum)

  def sortToOptimizedOrder(items: List[ContentsItem], capacity: Weight): List[ContentsItem] = {
    val algorithms: List[(List[ContentsItem]) => List[ContentsItem]] = List(sortToOptimizedOrderImpl)
    val resultsFromAlgorithms: List[List[ContentsItem]] = Nyyttimap.runAlgorithms(items, algorithms, capacity)
    resultsFromAlgorithms(0)
  }

  private def sortToOptimizedOrderImpl(items: List[ContentsItem]) : List[ContentsItem] = {
    items.sortWith((x, y) => { average(x) > average(y) })
  }
}

















