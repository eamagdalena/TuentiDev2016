package tuenti.challenge4

import tuenti.Utils._

object Hadouken extends App {

  case class Move (arrows : String, button : Char)

  val shortMoves = List( Move("D3R",'P'), Move("RD3",'P'), Move("D1L",'K')  )

  def stream3(str : String) : Stream[(String, Char)] = {
    if (str.length == 3)
      (str, 'Z')                           #:: Stream.Empty
    else
      (str.substring(0, 3), str.charAt(3)) #:: stream3(str.tail)
  }

  def solve(str: String): String = {

    val cleanedStr = str
      .replaceAll("RD","3").replaceAll("LD","1")
      .replaceAll("LU","7").replaceAll("RU","9")
      .replaceAll("-","")

    stream3(cleanedStr).foldLeft(0)( (B,A) => {
      if(shortMoves.exists( move => A._1 == move.arrows && A._2 != move.button)) B + 1
      else B
    }).toString
  }

  val res = for (line <- fromFile(args.head).drop(1)) yield solve(line).toString
  val output = (for (i <- 1 to res.length) yield "Case #" + i + ": " + res(i - 1)).toList
  toFile(pretty(output), s"${args.head}.solved")
}
