package fi.akisaarinen

import scala.actors.Actor
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

  def runAlgorithms(input: List[ContentsItem], algorithms: List[Algorithm], capacity: Weight): ResultsOfAlgorithms = {
    val s = self
    val actors = algorithms.map(a => new NyyttiActor(s, a, input, capacity).start)
    gather(actors, Nil)
  }

  private def gather(actors: List[Actor], accumulatedResults: ResultsOfAlgorithms): ResultsOfAlgorithms =
    actors match {
      case a :: as =>
        receive {
          case ret: List[ContentsItem] => gather(as, ret :: accumulatedResults)
        }
      case Nil => accumulatedResults
    }
}
