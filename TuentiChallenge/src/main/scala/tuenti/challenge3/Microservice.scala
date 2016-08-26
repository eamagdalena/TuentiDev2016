package tuenti.challenge3

import tuenti.Utils._
import tuenti.challenge3.model.{ExecutionState, Tape, ModelReader}

object Microservice extends App{

  val model = new ModelReader(args.head).readModel

  def solveTape (tape : Tape) : Tape = {
    model.code.execute(ExecutionState("start", tape, 0)).tape
  }

  def solve : List[Tape] = {
    model.tapes.map(solveTape _)
  }


  val output = solve.map( t => "Tape #" + t.num + ": " + t.data)
  toFile(pretty(output), s"${args.head}.solved")

}
