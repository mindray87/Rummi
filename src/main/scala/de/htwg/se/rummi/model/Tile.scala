package de.htwg.se.rummi.model

import play.api.libs.json._

case class Tile(number: Int, colour: RummiColour, joker : Boolean = false) {

  override def equals(that: Any): Boolean = {
    that match {
      case t: Tile => t.eq(this)
      case _ => false
    }
  }

  override def toString: String = {
    if (joker) {
      WHITE.stringInColor("J")
    } else {
      colour.stringInColor(number.toString)
    }
  }

  def tileToString: String = {
    var tile_number = ""
    if (!joker) {
      if (number > 9) {
        tile_number = "  " + number.toString
      }
      else {
        tile_number = "   " + number.toString
      }
      colour.name match {
        case "RED" => return tile_number + "R  "
        case "BLUE" => return tile_number + "B  "
        case "GREEN" => return tile_number + "G  "
        case "YELLOW" => return tile_number + "Y  "
        case "WHITE" => return tile_number + "W  "
      }
    }
    else {
      tile_number = "   J   "
    }
    tile_number
  }

  def toXml = {
    <tile>
      <number>
        {number}
      </number>
      <color>
        {colour}
      </color>
      <joker>
        {joker}
      </joker>
    </tile>
  }
}

object Tile {

  import play.api.libs.json._

  implicit val tileWrites = Json.writes[Tile]
  implicit val tileReads = Json.reads[Tile]
}

