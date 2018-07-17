

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

object coverageCalculator {
  private var weight : Int = 0
  private val table : TableStart= new TableStart()
  private val end_read : TableEnd= new TableEnd()


  private def tablePC(array: Array[String], para : Array[Double]) : Unit = {
    if (((array(1).toInt & 3) == 3) && (array(8).toInt > 0))
      table.add(array(3).toInt, array(7).toInt)
  }

  private def tableSC(array: Array[String], para : Array[Double]) : Unit = {
    if (((array(1).toInt & 3) == 3)) {
      if(array(8).toInt > 0)
        table.add(array(3).toInt, array(3).toInt + array(9).length)
      else
        table.add(array(3).toInt-array(9).length, array(3).toInt)
    }
  }

  private def tableSTD(array: Array[String], para : Array[Double]) : Unit = {
    if (((array(1).toInt & 3) == 3) && (array(8).toInt > 0) && array(8).toInt<para(0) && array(8).toInt>para(1))
      table.add(array(3).toInt, array(7).toInt)
  }


  private def createMPTable(sam:samFile, f:(Array[String],Array[Double]) => Unit, para: Array[Double] =Array[Double]()) : Unit = {
    for (line <- sam.iterator.dropWhile(x => x.charAt(0) == '@')) {
      val array = line.split("\\s")
           f(array,para)
    }
  }

  private def calculateCoverage(genome : Array[Int]) : Unit = {
    for(index <- 0 until genome.size){
      startController(index)
      genome(index)=weight
      endController(index)
    }
  }

  private def startController(index : Int) : Unit = {
    if(table.find(index)){
      var list = table.getEnds(index)
      for(end <- list){
        end_read.add(end)
        weight += 1
      }
    }
  }

  private def endController(index : Int) : Unit = {
    if(end_read.lookfor(index)){
      var counter : Int = end_read.getCounter(index)
      weight= weight - counter
      end_read.remove(index)
    }
  }

  private def createWigTrack(genome : Array[Int], nameFile: String) : Unit = {
    val buffer_writer : BufferedWriter= new BufferedWriter(new OutputStreamWriter(new FileOutputStream(nameFile),"UTF-8"))
    buffer_writer.write("fixedStep chrom=genome start=1 step=1 span=1" + "\n")
    try
    {
      for(i <- 0 until genome.size){
        buffer_writer.write(genome(i).toString + "\n")
      }
    }
    finally {
      buffer_writer.flush()
      buffer_writer.close()
    }
  }

  private def endProccess() : Unit = {
    table.clear()
    end_read.clear()
    weight=0
  }


  def getPhysicalCoverage(sam:samFile) : Unit = {
    val genome : Array[Int] = new Array[Int](sam.genoma_lenght)
    createMPTable(sam,tablePC)
    calculateCoverage(genome)
    createWigTrack(genome,"physical_coverage.wig")
    endProccess()
  }

  def getSequenceCoverage(sam:samFile) : Unit = {
    val genome : Array[Int] = new Array[Int](sam.genoma_lenght)
    createMPTable(sam,tableSC)
    calculateCoverage(genome)
    createWigTrack(genome,"sequence_coverage.wig")
  }

  def getStdTrack(sam : samFile, n: Int) : Unit = {
    val genome : Array[Int] = new Array[Int](sam.genoma_lenght)
    val tuple=StatisticsFunctionsContainer.getMeandStD(sam)
    val aboveMean : Double=tuple._1+(n*tuple._2)
    val belowMean : Double=tuple._1-(n*tuple._2)
    createMPTable(sam,tableSTD,Array(aboveMean,belowMean))
    calculateCoverage(genome)
    createWigTrack(genome,"stdTrack"+n+".wig")
    endProccess()
  }

  def getStdTrack(sam : samFile, n: Int, mean : Double, std:Double) : Unit = {
    val genome : Array[Int] = new Array[Int](sam.genoma_lenght)
    val aboveMean : Double=mean+(n*std)
    val belowMean : Double=mean.-(n*std)
    createMPTable(sam,tableSTD,Array(aboveMean,belowMean))
    calculateCoverage(genome)
    createWigTrack(genome,"stdTrack"+n+".wig")
    endProccess()
  }


}
