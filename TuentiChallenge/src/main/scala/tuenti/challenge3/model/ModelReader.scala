package tuenti.challenge3.model

import tuenti.Utils._

import scala.collection.mutable.{ListBuffer}

class ModelReader(inputFile : String) {

  val CONTINUE = true
  val STOP = false

  val INSTRUCTION_PREFIX = "      "
  val CASE_PREFIX = "    "
  val STATE_PREFIX = "  "
  val CODE_HEADER = "code:"
  val TAPES_HEADER = "tapes:"
  val EOF = "..."

  val lines = fromFile(inputFile).tail  /* Drop --- */

  def readModel : Model = {
      val code = parseCode
      Model(code, parseTapes(lines.dropWhile( _ != TAPES_HEADER)))
  }

  private def parseTapes(lines : List[String]) : List[Tape] = {
    lines
      .tail
      .takeWhile( (s :String) => s != EOF)
      .map( (x : String) => {
          val xx = x.split(":")
          Tape( xx(0).trim.toInt, xx(1).trim.replaceAll("'",""))
        }
      )
  }

  private def parseCode : Code = { /* Parser mode, FP is a hell for this */

    var currentCaseTrigger : String = null

    var currentStateName : String = null

    val states = new ListBuffer[State]()

    val cases = new ListBuffer[Case]()

    val instructions = new ListBuffer[Instruction]()

    def closeCase = {

      if(currentCaseTrigger != null){
        cases+= Case (currentCaseTrigger, instructions.toList)
        instructions.clear()
        currentCaseTrigger = null
      }

    }

    def closeState = {

      if(currentStateName != null){
        states += State(currentStateName, cases.toList.map(c => c.trigger -> c)(collection.breakOut))
        cases.clear
        currentStateName = null
      }
    }

    lines.tail.takeWhile( (s : String) => {

      if (s.startsWith(INSTRUCTION_PREFIX)){
        instructions += parseInstruction(s)
        CONTINUE
      }else if(s.startsWith(CASE_PREFIX)){
        closeCase
        currentCaseTrigger =  s.trim.replaceAll("'","").replaceAll(":", "")
        CONTINUE
      }else{
        closeCase
        closeState

        if(s.startsWith(STATE_PREFIX)){
          currentStateName = s.trim.replaceAll(":","")
          CONTINUE
        }else STOP
      }
   })

    Code ( states.toList.map( state => { state.name -> state })(collection.breakOut))
  }

  private def parseInstruction (str : String) : Instruction = {
    val split = str.split(":")

    split(0).trim match {

      case "move" => if ( split(1).trim == "right") MoveRight else MoveLeft
      case "write" => Write(split(1).trim.replaceAll("'",""))
      case "state" => ChangeState(split(1).trim)
    }

  }


}
