package tuenti.challenge7

import tuenti.challenge7.model.ProblemDescription

import tuenti.Utils._

object Tiles extends App{

	/* V2 */
  def expand (matrix : List[List[Int]]): List[List[Int]] = {
    matrix.map( li => li ++ li)
  }

	def expand2 (list : List[Int]) : List[Int] = {
		list.drop(2) ++ list
	}

	/* V3 */
	def fullExpansion (matrix : List[List[Int]]): List[List[Int]] = {
		println ("Expanding...")
		val expanded = matrix.map( li => li ++ li)

		expanded.drop(2) ++ expanded
	}

  def convertRowToSumsV3(row : List[Int], N : Int) : List[Int] = {

    def stream1(l : List[Int] , n : Int, acum : Int) : Stream[Int] = {
      (acum + l.head) #:: (if(n == N-2 || l.size == 1) Stream.Empty else stream1(l.tail, (n + 1), (acum + l.head)))
    }

    def f1(l : List[Int], n : Int) : List[Int] ={

      if(l.isEmpty || n > N) Nil else
        stream1(l,0,0).toList ++ f1(l.tail, (n + 1))
    }

    f1(row, 1)

  }

	/* V2 Only */
	def convertPass2(row : List[Int], N : Int) : Int = {

    val START : List[Int] = Nil

    var n : Int = 0 /* SORRY FP ! */

    row.foldRight(START){
      (cell : Int, res : List[Int]) => {

        n = n + 1

        val newSums: List[Int] =
          if(n < N) res.take(n-1).map( (x : Int) => (cell+x))
          else res.take(N-2).map( (x : Int) => (cell+x))

        val baggage = if(res.isEmpty) START  else res.max :: START

        cell :: (newSums ++ baggage)
      }
    }.max
	}

  /* V2: Phase 2 without transposition */
  def superPass2Turbo(matrix : List[List[Int]], N : Int) : Int = {

		def recursive ( matrix : List[List[Int]], currentMax : Int) : Int = {

			if(matrix.head.isEmpty){
				currentMax
			}else {
				val heads: List[Int] = expand2(matrix.map(_.head))
				val stepMax = convertPass2(heads, N)

				recursive(matrix.map(_.tail), if(currentMax > stepMax) currentMax else stepMax)

			}
		}

		recursive(matrix, 0)
  }

  def solve(problem :ProblemDescription) : String = {

		/* Some output to get feedback on whats happening over long executions */
		println("Solving N,M = " + problem.N + "," + problem.M)

    /* Look for infinites */
    val infiniteByRows = problem.matrix.exists( _.sum > 0)

    if(infiniteByRows){ /* Sorry FP! */
			println("Solved N,M = " + problem.N + "," + problem.M)
      println("Infinite by rows!")

			println("Case #" + problemNumber + ": INFINITY")
			problemNumber = problemNumber + 1

      return "INFINITY"
    }

    val infiniteByColumns = problem.matrix.transpose.exists(_.sum > 0)

    if(infiniteByColumns){ /* Sorry FP! */
			println("Solved N,M = " + problem.N + "," + problem.M)
      println("Infinite by columns!")

			println("Case #" + problemNumber + ": INFINITY")
			problemNumber = problemNumber + 1

      return "INFINITY"
    }

		/* 3.0 */
		val res: Int = fullExpansion(problem.matrix)
			.foldLeft(List[(Int,Int)]()) {
			(max: List[(Int,Int)], n: List[Int]) => {

				//TODO: Future-ama : Next step if performance was (still) not enough was to start zipping and folding futures

				val sums: List[Int] = convertRowToSumsV3(n, problem.M)

				if(max.isEmpty){
					sums.map( x => (x,x))
				}else{ /* MAGIC TIME!!! */
					(max zip sums).map{ z => {
						val newAcumulation = Math.max ( z._2 /* elem */ , z._2 /* elem */ + z._1._2 /* acum */)
						(Math.max (z._1._1 /* maxy */, newAcumulation), newAcumulation)
						}
					}
				}
			}
		}.map (_._1).max

		/* 2.0: Faster but more memory intensive: Not suitable for the two last problems */
//    val sumRows: List[List[Int]] = expand(problem.matrix).map(convertRowToSumsV3(_, problem.M))
//		println("Number of phase 2 iterations : " + sumRows.head.size)
//		println("Starting Phase 2 ...")
//		counter = 0
//
//		val res = superPass2Turbo(sumRows, problem.N)

		/* SOLUTION! */

		println("Solved N,M = " + problem.N + "," + problem.M)
		println("Case #" + problemNumber + ": " + res)
		problemNumber = problemNumber + 1

    if(res < 0) "0"
    else res.toString

  }

	/* ITERATE OVER ALL PROBLEMS */

	val PROBLEMS_TO_SKIP =  0
	var problemNumber = PROBLEMS_TO_SKIP + 1
	val MAX = 999

  val problemsToSolve = FileParser.parseInputFile(args.head)

	val solutions = problemsToSolve.drop(PROBLEMS_TO_SKIP).take(MAX).map(solve)
	val output = (for (i <- 1  to solutions.length) yield "Case #" + (i + PROBLEMS_TO_SKIP) + ": " + solutions(i - 1)).toList
	toFile(pretty(output), s"${args.head}.solved")

}
