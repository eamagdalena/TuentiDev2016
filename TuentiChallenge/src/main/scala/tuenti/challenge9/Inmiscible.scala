package tuenti.challenge9

import tuenti.Utils._

import scala.annotation.tailrec

case class ResultNotFoundException(stream: Stream[BigInt]) extends Throwable

object Inmiscible extends App{

	def oddNumbersStream(str : String) : Stream[BigInt] = {
		BigInt(str) #:: oddNumbersStream("1" + str)
	}

	def oddNumberStream(N : Int) = oddNumbersStream("1" * (1 + N / 500))

//	def oddNumbersStream(str : String) : Stream[BigInt] =
//		BigInt(str) #:: oddNumbersStream(str.tail)
//
//
//	def oddNumberStream(N : Int) = oddNumbersStream("1" * N)

	@tailrec
	def findSolution(N : Int, stream : Stream[BigInt]) : BigInt = {
		if(stream.head % N == 0) stream.head else findSolution(N, stream.tail)
	}

	@tailrec
	def recSolveZeros(N : Long, divisor : Int, numZeros : Long) :  (Long, Long) = {
		if(N % divisor != 0) (N, numZeros)
		else recSolveZeros(N / divisor, divisor, numZeros + 1)
	}

	def solveZeros(N : Long, divisor : Int) : (Long, Long) = recSolveZeros(N,divisor, 0)

	def solveOnes(N : Int) : Int = {
		findSolution(N, oddNumberStream(N)).toString.size

	}

	def isAlreadySolved(N : Long) : Boolean = N.toString.forall( c => c =='1' || c == '0')

	def extractSolution(N : Long) : (Long, Long) = {
		val Nstr = N.toString
		val counter = Nstr.count(_=='1')
		(counter, Nstr.size - counter)
	}

	def hintOne( N : Int) : Option[Long] = {
		None
//		if(N > 1 && BigInt("1" * (N-1)) % N == 0  ){
//			println("Hinted!")
//			Some(N-1)
//		}else None

	}

	def solve(N : Long) : String = {
		println("Solving " + N + "...")

		val solve10 = solveZeros(N, 10)
		val solve2 = solveZeros(solve10._1, 2)
		val solve5 = solveZeros(solve2._1, 5)

		val totalZeroes = solve10._2 + solve2._2 + solve5._2

		val Nodd : Int = solve5._1.toInt

		println("Pass one complete. Found: " + totalZeroes + " zeroes and N = " + Nodd)

		val hintSol = hintOne(Nodd)

		val res =
			if(! hintSol.isEmpty){
				(hintSol.get, totalZeroes)
		}else{
			(solveOnes(Nodd), totalZeroes)
		}

		println(N + " -> " + res)
		res._1 + " " + res._2
}

	val MAX = 100

	val start = System.currentTimeMillis()

	val res = for (line <- fromFile(args.head).drop(1).take(MAX)) yield solve(line.toLong).toString

	println("Solution took " + ((System.currentTimeMillis() - start) / 1) + " milliseconds")

	val output = (for (i <- 1 to res.length) yield "Case #" + i + ": " + res(i - 1)).toList
	toFile(pretty(output), s"${args.head}.solved")

}
