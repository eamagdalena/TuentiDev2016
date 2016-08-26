package tuenti.challenge3.model

case class Model(code : Code , tapes : List[Tape])

case class ExecutionState( currentState : String, tape: Tape , cursor : Int)

case class Code(states : Map[String, State]){

  def execute (executionState : ExecutionState) : ExecutionState = {

    def executionStream(exec : ExecutionState) : Stream[ExecutionState] = exec #:: executionStream(
      states(exec.currentState).execute(exec)
    )

    executionStream(executionState).find( _.currentState == "end").get
  }
}

case class Tape(num: Int, data : String)

case class State(name : String, cases : Map[String,Case]){

  def execute (executionState : ExecutionState) : ExecutionState = {

    val myCase = if (executionState.cursor >= executionState.tape.data.size) cases(" ")
    else cases(executionState.tape.data.charAt(executionState.cursor).toString)

    if(myCase == null) throw new RuntimeException ("Unexpected Execution State" + executionState)

    myCase.execute(executionState)

  }

}

case class Case(trigger : String , instructions : List[Instruction]){
  def execute (executionState : ExecutionState) : ExecutionState =
    instructions.foldLeft(executionState)( (execState, instruction) => instruction.execute(execState))
}

case class Write(s : String) extends Instruction

case class ChangeState(newState : String) extends Instruction

case object MoveLeft extends Instruction

case object MoveRight extends Instruction


trait Instruction {

  def execute (executionState: ExecutionState ) : ExecutionState = {

    this match {

      case Write (c) => {
        val myTape = executionState.tape
        val replaced : String = myTape.data.take(executionState.cursor) + c + myTape.data.drop(executionState.cursor + 1)

        executionState.copy(
          tape = myTape.copy (data = replaced)
        )

      }

      case MoveLeft => executionState.copy( cursor = executionState.cursor -1)

      case MoveRight => executionState.copy( cursor = executionState.cursor +1)

      case ChangeState (newState) => executionState.copy( currentState = newState)

    }

  }

}







