

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.util.Arrays.ArrayList

import scala.collection.mutable.ListBuffer
import scala.math.{pow, sqrt}


object StatisticsFunctionsContainer {

  def sortList(csv : csvFile) : ListBuffer[Int] = {
    val list : ListBuffer[Int] = new ListBuffer[Int]()
    for(line <- csv.iterator)
      list += line.toInt
    list.sorted
  }

  def cleanData(list: ListBuffer[Int]) : Unit = {
    //calc median and rage of data
    val mediana: Int = list.apply(((list.size)/2))
    val downLimit : Double = mediana/2
    val upLimit : Double = mediana * 1.5

    //find index of range
    var downIndex = list.indexWhere(n => n>downLimit)
    var upIndex = list.indexWhere(n => n>upLimit)

    //delete unuseful elements
    list.remove(upIndex,list.size-upIndex)
    list.remove(0,downIndex)
  }

  private def calc_mean (list: ListBuffer[Int]) : Double = {
    var mean : Double = 0
    for(n <-list)
      mean += n
    mean / list.size
  }

  private def calc_devStd(list: ListBuffer[Int],mean : Double) : Double = {
    var x : Double = 0
    for(n <-list)
      x += pow((n-mean),2)
    sqrt(x/list.size)
  }


  def getMeandStD(csv : csvFile) : Tuple3[Double,Double,Int] = {
    var order_list=sortList(csv)
    val realSize= order_list.size
    cleanData(order_list)
    val elemeDelete= realSize-order_list.size
    val mean=calc_mean(order_list)
    val std=calc_devStd(order_list,mean)
    Tuple3(mean,std,elemeDelete)
  }

  def getMeandStD(sam : samFile) : Tuple3[Double,Double,Int] = {
    getInsertsTrack(sam)
    var order_list=sortList(new csvFile("insert_length.csv"))
    val realSize= order_list.size
    cleanData(order_list)
    val elemeDelete= realSize-order_list.size
    val mean=calc_mean(order_list)
    val std=calc_devStd(order_list,mean)
    Tuple3(mean,std,elemeDelete)
  }

  def getInsertsTrack(sam: samFile) : Unit = {
    val buffer_writer : BufferedWriter= new BufferedWriter(new OutputStreamWriter(new FileOutputStream("insert_length.csv"),"UTF-8"))
    var tlen : String = ""
    try
    {
      for(line <- sam.iterator.dropWhile(x => x.charAt(0)=='@')){
        tlen=line.split("\\s")(8)
        if(tlen.toInt>0)
          buffer_writer.write(tlen + "\n")
      }
    }
    finally {
      buffer_writer.flush()
      buffer_writer.close()
    }
  }

}
