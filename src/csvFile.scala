


class csvFile(val name_file : String) extends Iterable[String] {

  override def iterator: Iterator[String] = {
    io.Source.fromFile(name_file).getLines()
  }

}
