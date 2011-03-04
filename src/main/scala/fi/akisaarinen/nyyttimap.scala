package fi.akisaarinen

import scala.actors.Actor
import scala.actors.Actor._

object Nyyttimap {
  def runAlgorithms(input: List[ContentsItem], algorithms: List[List[ContentsItem] => List[ContentsItem]], capacity: Weight): List[List[ContentsItem]] = {
    val s = self
    val actors = algorithms.map(a => actor { exec(s, a, input, capacity)})
    gather(actors, Nil)
  }

  private def gather(actors: List[Actor], accumulatedResults: List[List[ContentsItem]]): List[List[ContentsItem]] =
    actors match {
      case a :: as =>
        receive {
          case ret: List[ContentsItem] => gather(as, ret :: accumulatedResults)
        }
      case Nil => accumulatedResults
    }

  private def exec(replyTo: Actor, algorithm: List[ContentsItem] => List[ContentsItem], input: List[ContentsItem], capacity: Weight) = {
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
