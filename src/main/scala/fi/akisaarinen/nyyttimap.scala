package fi.akisaarinen

import scala.actors.Actor
import scala.actors.Actor._

object Nyyttimap {
  def runAlgorithms(input: List[ContentsItem], algorithms: List[List[ContentsItem] => List[ContentsItem]]): List[List[ContentsItem]] = {
    val s = self
    val actors = algorithms.map(a => actor { exec(s, a, input)})
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

  private def exec(replyTo: Actor, algorithm: List[ContentsItem] => List[ContentsItem], input: List[ContentsItem]) = {
    println("Hallo welt from Die Aktor!")
    replyTo ! algorithm(input)
  }
}
