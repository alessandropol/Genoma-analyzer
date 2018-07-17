

import java.io.FileNotFoundException


object genoma_analyzer {
  var live: Boolean = true

  private def request_file_sam(): samFile = {
    println("type a SAM file name (include extension) to analyze")
    new samFile(io.StdIn.readLine())
  }

  private def request_file_csv(): csvFile = {
    println("type a CSV file name (include extension) to analyze")
    new csvFile(io.StdIn.readLine())
  }

  private def insertsTrack(): Unit = {
    var file=request_file_sam()
    println("executing...")
    StatisticsFunctionsContainer.getInsertsTrack(file)
    println("process complete")

  }

  private def printMeanSTD(): Unit = {
    var file=request_file_csv()
    println("executing..." + "\n")
    val tuple=StatisticsFunctionsContainer.getMeandStD(file)
    println("Mean: " + tuple._1 +"\n"+"Standard Deviations: " + tuple._2 + "\n" + "Number inserts delete: " + tuple._3 + "\n")
  }



  private def stdTrackParameter() : Unit = {
    var file=request_file_sam()
    println("insert n")
    val n=io.StdIn.readInt()
    println("type: \n   p      to insert parameters manually (fast)\n   a      to calculate parameters automatically (slow)")
    val select=io.StdIn.readChar()
    select match {
      case 'p' => {
        println("insert mean")
        val mean=io.StdIn.readDouble()
        println("insert standard deviation")
        val std=io.StdIn.readDouble()
        println("executing...")
        coverageCalculator.getStdTrack(file,n,mean,std)
      }
      case 'a' => {
        println("executing...")
        coverageCalculator.getStdTrack(file,n)}
      case _ => wrongSelect()
      }
    println("process complete")
  }

  private def physicalCoverage() : Unit = {
    var file= request_file_sam()
    println("executing...")
    coverageCalculator.getPhysicalCoverage(file)
    println("process complete")
  }

  private def sequenceCoverage() : Unit = {
    var file= request_file_sam()
    println("executing...")
    coverageCalculator.getSequenceCoverage(file)
    println("process complete")
  }

  private def matchTest(request: Char): Unit = request match {
    case 'a' => insertsTrack()
    case 'b' => printMeanSTD()
    case 'c' => stdTrackParameter()
    case 'd' => physicalCoverage()
    case 'e' => sequenceCoverage()
    case 'x' => live=false
    case _ => println("\n                                                   >>>  wrong select  <<< \n")
  }

  def viewCommand(): Unit = {
    println("Type a letter and press enter:")
    println("       a    to get inserts length (inserts_length.csv)")
    println("       b    to calculate the mean and standard deviation of the inserts")
    println("       c    to get track with insert with a length exceeding N standard deviations above or below mean (stdTrackN.wig)")
    println("       d    to get physical coverage track (physical coverage.wgi)")
    println("       e    to get sequence coverage track (sequence coverage.wgi)"+ "\n")

    println("       x    to TERMINATE")
  }

  private def wrongSelect() : Unit = {
    println("\n                                     >>>  wrong file, repeat  <<< \n")

}




  def main(args: Array[String]): Unit = {
    println("WELCOME TO GENOMA ANALYZER\n")
    while (genoma_analyzer.live) {
      viewCommand()
      try{
        matchTest(io.StdIn.readChar())
      }
      catch {
        case e : FileNotFoundException => wrongSelect()
      }
    }
  }

}

