package tuenti

import Utils._

object TeamLunch extends App {

  def solve(num: Long): Long = {

    if(num == 0) 0
    else if(num <= 4) 1
    else if(num <= 6) 2
    else{
      Math.ceil((num.toDouble / 2)).toLong - 1
    }
  }

  val res = for (line <- fromFile(args.head).drop(1)) yield solve(line.toLong).toString

  val output = (for (i <- 1 to res.length) yield "Case #" + i + ": " + res(i - 1)).toList

  toFile(pretty(output), s"${args.head}.solved")

}