package fi.akisaarinen

import scala.actors.Actor
import scala.actors.TIMEOUT
import scala.actors.Actor._
import Nyyttimap._
import java.lang.String
import java.io._

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
  val logFile = new File("Nyyttilog-" + System.currentTimeMillis + ".txt")

  def max(x: Long, y: Long) = if(x >= y) x else y

  def runAlgorithms(input: List[ContentsItem], algorithms: List[Algorithm], capacity: Weight, timeOut: Long): ResultsOfAlgorithms = {
    new TimeoutActor(self, max(0, timeOut - safetyMarginMillis)).start
    if (Environment.debug) {
      withPrintWriter(
        logFile, writer => writer.println(new java.util.Date)
      )
    }
    algorithms.map(a => new NyyttiActor(self, a, input, capacity).start)

    def receiveNext(currentBest: ResultMessage): ResultsOfAlgorithms = {
      receive {
        case newResult: ResultMessage => {
          if (Environment.debug) {
            val logMessage = "Actor(" + newResult.name + ") returning: " + ValueUtils.calculateListValue(newResult.payload)
            println(logMessage)
            withPrintWriter(
              logFile, writer => writer.println(logMessage)
            )
          }
          if(ValueUtils.calculateListValue(newResult.payload) >= ValueUtils.calculateListValue(currentBest.payload)) receiveNext(newResult)
            else receiveNext(currentBest)
        }
        case TimeoutMessage => List(currentBest.payload)
      }
    }
    receiveNext(ResultMessage("empty", List[ContentsItem]()))
  }

  private def withPrintWriter(file: File, op: PrintWriter => Unit) {
    val writer = if (file.exists) {
      new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, true))), false)
          } else {
      new PrintWriter(file)
    }
    try {
      op(writer)
    } finally {
      writer.close()
    }
  }
}
