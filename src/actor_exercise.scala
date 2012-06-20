/**
 * Created by IntelliJ IDEA.
 * User: etuka
 * Date: 29/02/2012
 * Time: 00:33
 * To change this template use File | Settings | File Templates.
 */

import scala.util.Random
import scala.actors.Actor
import scala.actors.Actor._


class AvgRandGenerator(idx: Int, count: Int) extends Actor {
  val before = System.currentTimeMillis
  val rand = new Random(2*idx + 1)

  def act {
    receive {
      case collector: AvgCollector => {
        var partialSum = 0.0

        for (i <- 1 to count)
          partialSum += rand.nextDouble

        collector ! (idx, partialSum, count)
        println(idx.toString + " sent " + count.toString + " items.")

        val after = System.currentTimeMillis
        println(idx.toString + " sent after " + ((after - before)/1000.0).toString + " seconds.")

        exit
      }
    }
  }
}

class AvgCollector(total: Int) extends Actor {
  val before = System.currentTimeMillis
  var count = 0
  var aggregate = 0.0

  def act {
    while(true){
      receive {
        case (idx: Int, increment: Double, incCount: Int) => {
          count += incCount
          aggregate += increment
          if (count == total) {
            println("Average = " + (aggregate/total).toString)
            val after = System.currentTimeMillis
            println(idx.toString + " received " + incCount.toString + " items, " + total.toString + " in total.")
            println("Finished after " + ((after - before)/1000.0).toString + " seconds.")
            exit
          }
          println(idx.toString + " received " + incCount.toString + " items.")
        }
        case _ => {
          println("Don't know what to do with this one...")
        }
      }
    }
  }
}

object ActorExerciseMain extends App {
  val before = System.currentTimeMillis
  val rand = Random
  val simCount = 1000000000
  var aggregate = 0.0
  val split = 4

  val effSimCount = simCount - simCount % split

  for (i <- 1 to effSimCount)
    aggregate += rand.nextDouble

  val average = aggregate / effSimCount

  println("average: " + average.toString)
  val after = System.currentTimeMillis
  println(((after - before)/1000.0).toString + " seconds, " + effSimCount.toString + " items.")

  val generator = for (i <- 1 to split) yield new AvgRandGenerator(i, effSimCount / split)
  val collector = new AvgCollector(effSimCount)

  collector.start

  for (i <- 0 to split-1) {
    generator(i).start
    generator(i) ! collector
  }

}
