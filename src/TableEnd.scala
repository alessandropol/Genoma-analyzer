

import scala.collection.mutable.HashMap

class TableEnd {
  private val tree : HashMap[Int,Int] = new HashMap[Int,Int] ()

  def add(index_end: Int) : Unit ={
    if(tree.contains(index_end)){
      var counter : Int = tree(index_end)
      tree.remove(index_end)
      tree += index_end -> (counter+1)
    }
    else{
      tree += index_end -> 1
    }
  }

  def remove(index: Int) : Unit = {
    tree.remove(index)
  }

  def lookfor(ele : Int) : Boolean = {
    tree.contains(ele)
  }

  def getCounter(index_end: Int) : Int = {
    tree(index_end)
  }

  def clear() : Unit = {
    tree.clear()
  }

}
