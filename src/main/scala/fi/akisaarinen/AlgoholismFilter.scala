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
import java.lang.String

class AlgoholismFilter extends ScalatraFilter {

  get("/") {
    <html>
      <body>
        <h1>Use POST</h1>
      </body>
    </html>
  }
  
  post("/") {
    val input = getBody(request.body, params.keys)
    if (input.size > 0) {
      val json = parse(input)
      contentType = "application/json"
      compact(process(json)) + "\n"
    } else {
      "{}\n"
    }
  }

  notFound {
    response.setStatus(404)
    <html>
      <body>
        <h1>404</h1>
      </body>
    </html>
  }

  private def getBody(body: String, paramNames: Iterable[String]): String = {
    // Sometimes scalatra seems to eat the body and put the content to first
    // key of the map. I didn't want to invest time on finding out why, this'll do :-)
    if (paramNames.size > 0) paramNames.head else body
  }

  private def process(json: JsonAST.JValue): text.Document = {
    import net.liftweb.json.JsonAST._
    import net.liftweb.json.JsonDSL._
    
    parseRequestObjectFrom(json) match {
      case Some(o) => render(findItemsFrom(o))
      case None => render(List())
    }
  }

  private def parseRequestObjectFrom(json: JsonAST.JValue): Option[KnapsackRequest] = {
    implicit val formats = net.liftweb.json.DefaultFormats
    implicit def listToWeight(list: List[Int]): Weight = Weight(list)
    try {
      Some(json.extract[KnapsackRequest]);
    } catch {
      case _ => None
    }
  }

  private def findItemsFrom(req: KnapsackRequest): List[String] = {
    val controller = new ActorController
    val results = controller.chooseItemsToKnapsack(req.contents, req.capacityAsWeight, req.timeout)
    if (Environment.debug) {
      val message: String = "Total value: " + ValueUtils.calculateListValue(results)
      println(message)
      Nyyttimap.withPrintWriter(
        Nyyttimap.logFile, writer => writer.println(message)
      )
    }
    results.map(_.id)
  }
}

object Environment {
  val debug = System.getProperty("nyytti.debug") match {
    case value: String => value.toBoolean
    case _ => false
  }
  println("Server debug = " + debug)
}

case class Weight(d: List[Int]) {
  val dimensions = d

  def fits(other: Weight): Boolean = dimensions.zip(other.dimensions).filter( x => { (x._1 < x._2) } ).isEmpty

  def plus(other: Weight): Weight = Weight(dimensions.zip(other.dimensions).map(x => x._1 + x._2))
}

case class ContentsItem(id: String, weight: List[Int], value: Int) {
  def contentsWeight = Weight(weight)
}

case class NormalizedContentsItem(item: ContentsItem, totalWeight: Double, value: Double) {

}

case class KnapsackRequest(name: String, timeout: Int, contents: List[ContentsItem], capacity: List[Int]) {
  def capacityAsWeight = Weight(capacity)
}
