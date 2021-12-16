package scalashop

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

object IndividualTask extends App {

  def part: PartialFunction[Int, Double] = new PartialFunction[Int, Double] {
    override def apply(v1: Int): Double = {
      if (v1 < 55) fact(v1) + v1
      else v1
    }

    @tailrec
    def fact(x: Double, acc: Double = 1): Double = {
      if (x == 0) acc
      else fact(x - 1, acc * x)
    }

    override def isDefinedAt(x: Int): Boolean = x >= 0 && x != 55
  }

  def printResult(tup: (Int, Int)): Unit = {
    val range = tup._1 to tup._2
    range.foreach(i => println(i.toString + " " + part.lift(i).toString))
  }


  def checkSeqParallel(): Unit = {
    val strips = splitRange(-250 to 250, 10)
    val tasks = strips.map(i => task(printResult(i)))
    tasks.foreach(i => i.join())
  }

  def splitRange(list: Range, numTasks: Int): ParSeq[(Int, Int)] = {

    val range = if(numTasks < list.length){
      list.head to list.end by list.length/numTasks
    }
    else{
      list.head to list.end by 1
    }

    range.zip(range.tail).par
  }

  checkSeqParallel()
}
