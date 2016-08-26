package tuenti.challenge2

import tuenti.Utils._
import scala.io.Source

object Manuscript extends App{

  /* Small enough to have it in memory */
  lazy val corpus = Source.fromFile("input/challenge2/corpus.txt").getLines.toList.flatMap(_.split(" "))

  def solve( A: Int, B: Int) : String = {

    corpus
      .drop(A - 1)  /* Take the subset we want */
      .take(B - A + 1)
      .groupBy(x => x) /* Count words */
      .map(x => (x._2.size, x._2.head))
      .toList.sortBy(x => -1 * x._1) /* Order */
      .take(3) /* Top 3 */
      .map(x => x._2 + " " + x._1) /*  Format Output */
      .mkString(",")
   }

  /* --- INPUT / OUTPUT --- */

  def solve(str : String) : String = {
    val split = str.split(" ")
    solve(split.head.toInt, split.tail.head.toInt)
  }

  val res = for (line <- fromFile(args.head).drop(1)) yield solve(line).toString

  val output = (for (i <- 1 to res.length) yield "Case #" + i + ": " + res(i - 1)).toList

  toFile(pretty(output), s"${args.head}.solved")

}
