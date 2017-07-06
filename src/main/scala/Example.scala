import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

import scala.util.Random


// To use this example, open the SBT console and type:
//
// Example.image.draw
object InBound{
  def row(number: Int): Int =
    number match {
      case (-1) => number + 480
      case 480 => number - 480
      case n => n
    }
  def column(number: Int): Int =
    number match {
      case (-1) => number + 640
      case 640 => number - 640
      case n => n
    }
}

case class Rules(alive_to_alive: List[Int], dead_to_alive: List[Int])

case class GameTable(table: Array[Array[Int]], rules: Rules){
  def update : GameTable = {
    var copied : Array[Array[Int]] = table;
    for (i <- 0.to(479); j <- 0.to(639)){
      var neighbours: Int = List(
          table(InBound.row(i-1))(InBound.column(j-1)),
          table(InBound.row(i-1))(j),
          table(InBound.row(i-1))(InBound.column(j+1)),

          table(i)(InBound.column(j-1)),
          table(i)(InBound.column(j+1)),

          table(InBound.row(i+1))(InBound.column(j-1)),
          table(InBound.row(i+1))(j),
          table(InBound.row(i+1))(InBound.column(j+1))).sum

      Tuple3(copied(i)(j), rules.alive_to_alive contains neighbours, rules.dead_to_alive contains neighbours) match {
        case Tuple3(1,false, _) => copied(i)(j) = 0
        case Tuple3(0,_,true) => copied(i)(j) = 1
        case _ => {}
      }
    }
    new GameTable(copied, rules)
  }

}

object RandomValue{
  def get = {
    val generator: List[Int] = List(0,0,0,0,0,0,0,1,1,1)
    Random.shuffle(generator).head
  }
}

object TableGenerator{
  def generate: GameTable =
    new GameTable(
      1.to(480).toArray.map { (_ : Int) => 1.to(640).toArray.map { (_ : Int) => RandomValue.get} },
      new Rules(List(1,2), List(3,4)) )
}
