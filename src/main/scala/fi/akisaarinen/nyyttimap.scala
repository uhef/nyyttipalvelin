package fi.akisaarinen

import scala.actors.Actor
import scala.actors.TIMEOUT
import scala.actors.Actor._
import Nyyttimap._

class NyyttiActor(controller: Actor, algorithm: Algorithm, input: List[ContentsItem], capacity: Weight) extends Actor {
  def act() {
    println("Hallo welt from Die Aktor!")
    algorithm.pack(input, capacity, controller)
  }
}

case class TimeoutMessage()
case class ResultMessage(name: String, payload: List[ContentsItem])

class TimeoutActor(controller: Actor, timeOut: Long) extends Actor {
  def act() {
    receiveWithin(timeOut) {
      case TIMEOUT => println("Send shutdown!"); controller ! TimeoutMessage
    }
  }
}

object Nyyttimap {
  type SortAlgorithm = List[ContentsItem] => List[ContentsItem]
  type ResultsOfAlgorithms = List[List[ContentsItem]]

  val safetyMarginMillis = 10000;

  def max(x: Long, y: Long) = if(x >= y) x else y

  def runAlgorithms(input: List[ContentsItem], algorithms: List[Algorithm], capacity: Weight, timeOut: Long): ResultsOfAlgorithms = {
    new TimeoutActor(self, max(0, timeOut - safetyMarginMillis)).start
    algorithms.map(a => new NyyttiActor(self, a, input, capacity).start)

    def receiveNext(currentBest: ResultMessage): ResultsOfAlgorithms = {
      receive {
        case newResult: ResultMessage => {
          if (Environment.debug) {
            println("Actor(" + newResult.name + ") returning: " + ValueUtils.calculateListValue(newResult.payload))
          }
          if(ValueUtils.calculateListValue(newResult.payload) >= ValueUtils.calculateListValue(currentBest.payload)) receiveNext(newResult)
            else receiveNext(currentBest)
        }
        case TimeoutMessage => List(currentBest.payload)
      }
    }
    receiveNext(ResultMessage("empty", List[ContentsItem]()))
  }
}
