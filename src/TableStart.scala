

import scala.collection.mutable.{HashMap,ListBuffer}

class TableStart {
  private val table : HashMap[Int,ListBuffer[Int]] =  new HashMap[Int,ListBuffer[Int]]()

  def add(start: Int, end :Int) : Unit = {
    if(table.contains(start)){
      table(start) += end
    }
    else{
      var endList=new ListBuffer[Int]
      endList += end
      table += start -> endList
    }
  }

  def find(index : Int) : Boolean = {
    table.contains(index)
  }

  def getEnds(star: Int) : ListBuffer[Int] = {
    table(star)
  }

  def print() : Unit = {
    table.foreach(println)
  }

  def clear() : Unit = {
    table.clear()
  }
}
