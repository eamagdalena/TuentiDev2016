import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


val f = (1 to 10).map(x => Future[Int](x))
	.foldLeft(Future[(Int, Int)]((0,1))){

	(fold, elem) => {

		(fold zip elem).map{
			x=> {
				( x._1._1 + x._2, x._1._2 * x._2)
			}
		}
	}
}

Await.result(f, 1 seconds)

//val f = (1 to 10).map(x => Future[Int](x)).reduce{
//
//	(f1, f2) => {
//
//		val v: Future[Int] = (f1 zip f2).map( x => x._1 + x._2 )
//
//		v
//	}
//
//}



