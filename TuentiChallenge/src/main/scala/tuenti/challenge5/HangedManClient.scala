package tuenti.challenge5

/**
 * Created by Eduardo on 27/04/2016.
 */
class HangedManClient {

	val client = new SocketClient("52.49.91.111", 9988)

	var problemNumber = 0;

	def startProblem : String = {
		problemNumber = problemNumber + 1;
		println("Starting problem number " + problemNumber)
		client.enter()
		Thread.sleep(200)
		client.getLastRelevantMessage.trim.replaceAll(" ","")
	}

	def guess (char : Char) : Option[String] = {
		println("Guessing " + char)

		client.write(char)
		Thread.sleep(200)

		val result = client.getLastRelevantMessage
		println("Hanged Man replied with " + result)

		if(result.contains("Get ready"))
			None
		else if (result.contains("_"))
			Some(result.trim.replaceAll(" ",""))
		else if (result.contains("Your submit key is")){
			println("WE WON! : "+ result)
			client.close
			System.exit(0)
			None
		}else{
			println("We failed Captain :(")
			client.close
			System.exit(1)
			None
		}

	}

}
