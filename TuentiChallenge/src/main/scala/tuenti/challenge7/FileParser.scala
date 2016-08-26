package tuenti.challenge7

import tuenti.challenge7.model.ProblemDescription

object FileParser {

  def parseInputFile(filePath : String) : List[ProblemDescription] = {

    val file = tuenti.Utils.fromFile(filePath)

    parse(file.tail)
  }

  private def parse(input : List[String]) : List[ProblemDescription] = {

    input.headOption match {

      case None => Nil

      case Some(str) => {
        val head = input.head.split(" ")
        val N = head(0).toInt
        val M = head(1).toInt

        ProblemDescription(N, M,

        input.tail
          .take(N)
          .map(
            s => s.map( (c: Char) => Constants.CHAR_TABLE(c)).toList
          )

        ) :: parse(input.drop(N + 1))

      }
    }
  }


}
