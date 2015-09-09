package org.vlinderlang.vlinderc

import com.google.common.io.Files
import java.io.File

object Main {
  def main(args: Array[String]): Unit =
    if (args.size == 0) {
      usage()
    } else {
      val inputDirs :+ outputDir = args.toSeq
      val allSources = inputDirs map { i => (inputDirs, sources(new File(i))) }
    }

  def usage() = {
    println("usage: vlinderc [flag ...] [input-dir ...] output-dir")
    println("")
    println("Currently, no flags are available.")
  }

  def sources(directory: File): Set[File] = {
    val (files, directories) = directory.listFiles.toSet.partition(_.isDirectory)
    files.filter{f => Files.getFileExtension(f.toString) == "vl"} ++ directories flatMap sources
  }
}
