package fi.akisaarinen

import scala.actors.Actor
import scala.actors.TIMEOUT
import scala.actors.Actor._
import Nyyttimap._

class NyyttiActor(controller: Actor, algorithm: Algorithm, input: List[ContentsItem], capacity: Weight) extends Actor {
  def act() {
    exec(controller, algorithm, input, capacity)
  }

  def exec(replyTo: Actor, algorithm: Algorithm, input: List[ContentsItem], capacity: Weight) = {
    println("Hallo welt from Die Aktor!")
    val fullSortedResultList: List[ContentsItem] = algorithm(input)
    val resultsToFillKnapsack = iterateUntilFull(capacity, Nil, fullSortedResultList)
    replyTo ! resultsToFillKnapsack
  }

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

case class Timeout()

class TimeoutActor(controller: Actor, timeOut: Long) extends Actor {
  def act() {
    receiveWithin(timeOut) {
      case TIMEOUT => println("Send shutdown!"); controller ! Timeout
    }
  }
}

object Nyyttimap {
  type Algorithm = List[ContentsItem] => List[ContentsItem]
  type ResultsOfAlgorithms = List[List[ContentsItem]]

  val safetyMarginMillis = 3000;

  def max(x: Long, y: Long) = if(x >= y) x else y

  def runAlgorithms(input: List[ContentsItem], algorithms: List[Algorithm], capacity: Weight, timeOut: Long): ResultsOfAlgorithms = {
    new TimeoutActor(self, max(0, timeOut - safetyMarginMillis)).start
    algorithms.map(a => new NyyttiActor(self, a, input, capacity).start)

    def receiveNext(currentBest: List[ContentsItem]): ResultsOfAlgorithms = {
      receive {
        case newResult: List[ContentsItem] => {
          if(ValueUtils.calculateListValue(newResult) >= ValueUtils.calculateListValue(currentBest)) receiveNext(newResult)
            else receiveNext(currentBest)
        }
        case Timeout => List(currentBest)
      }
    }
    receiveNext(List[ContentsItem]())
  }
}
