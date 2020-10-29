package de.htwg.se.rummi.model
import de.htwg.se.rummi.Const
import play.api.libs.json.{JsNumber, JsObject, Json}

case class Grid(ROWS: Int, COLS: Int, tiles: Map[(Int, Int), Tile]) {

  tiles.keys.find(x => x._1 > ROWS || x._2 > COLS)
    .map(x => throw new IllegalArgumentException("Tile indices '" + x + "' out of bounds."))

  def toStringField: String = {
    var field = ""
    val letters = (('A' to ('A' + Const.GRID_COLS - 1).toChar).mkString("        ", "       ", "\n"))
    val lineseparator = "    x-------" + ("x-------" * (Const.GRID_COLS -1)) + "x\n"
    val line = "    |       " + ("|       " * (Const.GRID_COLS -1)) + "|\n"

    field = letters
    for (numLine <- 1 to Const.GRID_ROWS) {
      val lineWithContent = "  " + String.format("%s", numLine.toString) + " |   0   " + ("|   0   " * (Const.GRID_COLS -1)) + "|\n"
      field = field + lineseparator + line + lineWithContent + line
    }
    field = field + lineseparator + "\n\n"
    val separatorOfGrids = " _" + "_" * 110 + "\n\n\n"
    for {
      row <- 1 until Const.GRID_ROWS + 1
      col <- 1 until Const.GRID_COLS + 1
    } if (tiles.get(row,col).isEmpty) {
      field = field.replaceFirst("   0   ", "       ")
      }
      else {
      field = field.replaceFirst("   0   ", tiles(row, col).tileToString)
      }
    field + separatorOfGrids
  }

  def toStringRack: String = {
    var fieldRack = ""
    val lineseparatorRack = "    x-------" + ("x-------" * (Const.RACK_COLS -1)) + "x\n"
    val lineRack = "    |       " + ("|       " * (Const.RACK_COLS -1)) + "|\n"

    for (numbLine <- (Const.GRID_ROWS + 1) to (Const.GRID_ROWS + Const.RACK_ROWS)) {
      if (numbLine < 10) {
        val lineRackWithContent = "  " + String.format("%s", numbLine.toString) + " |   0   " + ("|   0   " * (Const.RACK_COLS -1)) + "|\n"
        fieldRack = fieldRack + (lineseparatorRack + lineRack + lineRackWithContent + lineRack)
      }
      else {
        val lineRackWithContent = " " + String.format("%s", numbLine.toString) + " |   0   " + ("|   0   " * (Const.RACK_COLS -1)) + "|\n"
        fieldRack = fieldRack + lineseparatorRack + lineRack + lineRackWithContent + lineRack
      }
    }
    fieldRack = fieldRack + lineseparatorRack + "\n\n\n"

    for {
      row <- 1 until Const.RACK_ROWS + 1
      col <- 1 until Const.RACK_COLS + 1
    } if (tiles.get(row, col).isEmpty) {
      fieldRack = fieldRack.replaceFirst("   0   ", "       ")
    }
      else {
      fieldRack = fieldRack.replaceFirst("   0   ", tiles(row, col).tileToString)
      }
    fieldRack
  }

  def getTileAt(row: Int, col: Int): Option[Tile] = {
    tiles.get((row, col))
  }

  def getFreeField(): Option[(Int, Int)] = {
    for (i <- 1 to ROWS; j <- 1 to COLS) {
      if (tiles.get((i, j)).isEmpty) {
        return Some(((i, j)))
      }
    }
    None
  }

  /**
    * Get the position of a tile in the grid.
    *
    * @param tile the tile
    * @return row, col tuple
    */
  def getTilePosition(tile: Tile): Option[(Int, Int)] = tiles.find(x => x._2 == tile).map(x => x._1)

  def size(): Int = tiles.size

  /**
    * Creates a new Grid with same size but with the specified tiles.
    * @param tiles the tiles for the new Grid
    * @return a new Grid with same size
    */
  def copy(tiles: Map[(Int, Int), Tile]): Grid ={
    Grid(ROWS,COLS, tiles)
  }

  def toXml = {
    <grid>
      <cols>{COLS}</cols>
      <rows>{ROWS}</rows>
      <tiles>
        {tiles.toList.map(mapTupleToXml)}
      </tiles>
    </grid>
  }

  private def mapTupleToXml(tuple: ((Int, Int), Tile)) = {
    val x = tuple._1._1
    val y = tuple._1._2
    val t = tuple._2

    <tilePos>
      <x>{x}</x>
      <y>{y}</y>{t.toXml}
    </tilePos>
  }
}

object Grid {

  import play.api.libs.json._

  implicit val mapWrites: Writes[Grid] = new Writes[Grid] {
    override def writes(o: Grid): JsValue = Json.obj(
        "COLS" -> JsNumber(o.COLS),
        "ROWS" -> JsNumber(o.ROWS),
        "tiles" -> JsArray(o.tiles.toList.map(mapTupleToJson))
    )
  }

  def mapTupleToJson(tuple: ((Int, Int), Tile)): JsObject = {
    val x = tuple._1._1
    val y = tuple._1._2
    val t = tuple._2
    Json.obj(
      "x" -> JsNumber(x),
      "y" -> JsNumber(y),
      "tile" -> Json.toJson(t)
    )
  }

}