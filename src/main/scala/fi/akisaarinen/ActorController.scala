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

class ActorController {
  def filterFittingItems(items: List[ContentsItem], capacity: Weight) : List[ContentsItem] = {
    items.filter(x => { capacity.fits(x.contentsWeight) } )
  }
}

















