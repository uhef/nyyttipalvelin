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

object Nyyttimap {
  type Algorithm = List[ContentsItem] => List[ContentsItem]
  type ResultsOfAlgorithms = List[List[ContentsItem]]

  val safetyMarginMillis = 3000;

  def runAlgorithms(input: List[ContentsItem], algorithms: List[Algorithm], capacity: Weight, timeOut: Long): ResultsOfAlgorithms = {
    val startTime = System.currentTimeMillis
    val s = self
    val actors = algorithms.map(a => new NyyttiActor(s, a, input, capacity).start)
    gather(actors, Nil, startTime, timeOut)
  }

  private def gather(actors: List[Actor], accumulatedResults: ResultsOfAlgorithms, startTime: Long, timeOut: Long): ResultsOfAlgorithms =
    actors match {
      case a :: as => {
        val elapsedMillis = System.currentTimeMillis - startTime
        val remainingMillis = timeOut - safetyMarginMillis - elapsedMillis
        //println("Remaining: " + remainingMillis)
        if (remainingMillis < safetyMarginMillis) {
          return accumulatedResults
        }
        receiveWithin(remainingMillis) {
          case ret: List[ContentsItem] => gather(as, ret :: accumulatedResults, startTime, timeOut)
          case timeout: AnyRef => println(timeout); accumulatedResults
        }
      }
      case Nil => accumulatedResults
    }
}
