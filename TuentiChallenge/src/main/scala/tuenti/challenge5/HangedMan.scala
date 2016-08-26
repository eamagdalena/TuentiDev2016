package tuenti.challenge5

import tuenti.Utils._

case class SolutionState(
													matches : List[String],
													partialWord : String,
													guess : Char,
													usedLetters : List[Char]
													)

object HangedMan extends App{

	val classifiedDictionary: Map[Int, List[String]] =
		fromFile("input/challenge5/words.txt").groupBy(_.length)

	val letterFrequenciesByWordLength: Map[Int, List[(Int, Char)]] =
		classifiedDictionary.map( (x: (Int, List[String])) => {
		(x._1, convertWordListToFrequenciesList(x._2))
	})

	def convertWordListToFrequenciesList ( words : List[String]) = { /* Magic! */
		words
			.distinct /* Because we don't care how many times a letter repeats on a word */
			.flatten
			.groupBy( x => x)
			.mapValues(_.size)
			.toList
			.map ( (x: (Char, Int)) => (x._2, x._1))
			.sortBy(-1 * _._1)
	}

	def filterList ( list : List[String], filter : String) : List[String] = {
		val reFilter = filter.replaceAll("_",".")
		list.filter(_.matches(reFilter))
	}

	def negativeFilterList (list : List[String], char : Char) : List[String] = {
		list.filter( ! _.contains(char))
	}

	def selectGuess (frequenciesList : List[(Int, Char)], wordCount : Int): Char = {
		frequenciesList.head._2
	}

	def nextGuess (state: SolutionState) : SolutionState = {

		if(state.matches == Nil){ /* First pass */

			val size = state.partialWord.length
			val guess = letterFrequenciesByWordLength(size).head._2

			/* Initial State */
			SolutionState(
				classifiedDictionary(size),
				state.partialWord,
				guess,
				guess :: Nil
			)
		}else{

			val lastGuessWasCorrect = state.partialWord.contains(state.guess)

			val wordList: List[String] =
				if(lastGuessWasCorrect) filterList(state.matches, state.partialWord)
				else negativeFilterList(state.matches, state.guess)

			println("Possible words: " + wordList.size)

			val frequenciesList: List[(Int, Char)] =
				convertWordListToFrequenciesList(wordList)
					.filter( (x: (Int, Char)) => ! state.usedLetters.contains(x._2))

			val guess = selectGuess(frequenciesList, wordList.size)

			SolutionState(
				wordList,
				state.partialWord,
				guess,
				guess :: state.usedLetters
			)

		}

	}

	val client = new HangedManClient

	def solveProblem(solutionState: SolutionState): Unit ={

		client.guess(solutionState.guess) match {

			case None =>
				println ( "Solved! Word was " + solutionState.partialWord.replaceAll("_", "" + solutionState.guess))
			case Some(str) =>
				solveProblem(
					nextGuess(
						solutionState.copy( partialWord = str)
					)
				)
		}
	}

	def initSolveProblem(): Unit = {
		val init = client.startProblem

		solveProblem {
			nextGuess {
				SolutionState(Nil, init, ' ', Nil)
			}
		}
	}

	while(true){
		initSolveProblem
	}
}
