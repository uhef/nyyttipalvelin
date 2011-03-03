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

class AlgoholismFilter extends ScalatraFilter {

  before {
    ProcessingActor.start()
  }

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
        case Some(o) => { render(findItemsFrom(o)); }
        case None => { println("no match"); render(List()); }
    }
    /*
    json \\ "a" match {
      case JField("a", JString("lol")) => render(List(1,3))
      case _ => render(List())
    } */
  }

  private def parseRequestObjectFrom(json: JsonAST.JValue): Option[KnapsackRequest] = {
    implicit val formats = net.liftweb.json.DefaultFormats
    implicit def listToWeight(list: List[Int]) : Weight = Weight(list)

    try {
      val o = json.extract[KnapsackRequest]
      return Some(o);
    } catch {
      case ex: net.liftweb.json.MappingException => println("Parse error"); return None
    }
  }

  private def findItemsFrom(req: KnapsackRequest) : List[String] = {
    val controller = new ActorController
    val filtered = controller.filterFittingItems(req.contents, req.capacityAsWeight)
    filtered match {
      case Some(l) => List(l.head.id)
      case None => List()
    }
  }
}

case class Weight(dimensions: List[Int]) {
  def fits(other: Weight) : Boolean = {
    val dimensionComparisonTuples = dimensions.zip(other.dimensions)
    dimensionComparisonTuples.filter( x => { (x._1 < x._2) } ).isEmpty
  }
}

case class ContentsItem(id: String, weight: List[Int], value: Int) {
  def contentsWeight = Weight(weight)
}

case class KnapsackRequest(name: String, timeout: Int, contents: List[ContentsItem], capacity: List[Int]) {
  def capacityAsWeight = Weight(capacity)
}

object ProcessingActor extends Actor {
  def act() {
    loop {
      react {
        case (cField: ContentsItem) => println("Got C field with id " + cField.id)
        case msg => println("Unhandled message: " + msg)
      }
    }
  }
}

case class Enqueue(message: Any)

case class Dequeue(actor: Actor)

object ProcessingQueue extends Reactor[Any] {
  import scala.collection.mutable.Queue

  private val queue = new Queue[Any]

  def act() {
    loop {
      react {
        case Enqueue(d) => queue enqueue d
        case Dequeue(a) if queue.nonEmpty => a ! (queue dequeue)
        }
    }
  }
}

