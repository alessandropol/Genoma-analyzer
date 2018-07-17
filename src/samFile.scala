

class samFile (val name_file : String) extends Iterable[String]{


  val genoma_name = genomaName()
  val genoma_lenght : Int= genomaLength()

  private def genomaLength () : Int = {
    val it=iterator
    if(it.hasNext){
      val header : String = it.next()
      if(header.startsWith("@") && header.contains("LN"))
            return header.split("LN:")(1).toInt
    }
    return 0
  }

  private def genomaName() : String = {
    val it=iterator
    if(it.hasNext){
      val header : String = it.next()
      if(header.startsWith("@") && header.contains("SN"))
        return header.split("SN:")(1)
    }
    return "no_name"
  }


  override def iterator: Iterator[String] = {
    io.Source.fromFile(name_file).getLines()
  }
}
