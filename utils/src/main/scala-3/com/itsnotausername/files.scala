package com.itsnotausername

import scala.io.Source
import scala.util.Using

object files:

  def loadFile(fileName: String): Seq[String] =
    val src = Source.fromResource(fileName)
    Using.resource(src)(_.getLines().toSeq)

end files
