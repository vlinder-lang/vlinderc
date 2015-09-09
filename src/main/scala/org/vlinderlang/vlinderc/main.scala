package org.vlinderlang.vlinderc

import com.google.common.io.Files
import java.io.File
import scala.io

object Main {
  def main(args: Array[String]): Unit =
    if (args.size == 0) {
      usage()
    } else {
      val inputDirs :+ outputDir = args.toSeq.map(new File(_))
      val moduleSources = readSources(inputDirs.toSet)
      val modules = parseModules(moduleSources)
      name.resolve(modules)
      `type`.analyze(modules)
      println(modules)
    }

  def usage() = {
    println("usage: vlinderc [flag ...] [input-dir ...] output-dir")
    println("")
    println("Currently, no flags are available.")
  }

  private def readSources(inputDirs: Set[File]): Map[ModuleName, String] =
    (for {
      inputDir <- inputDirs
      sourceFile <- sources(inputDir)
    } yield {
      val filename = sourceFile.toString.substring(inputDir.toString.size + 1)
      val moduleName = {
        var n = filename.replace('/', '.').replace('\\', '.')
        n = n.substring(0, n.size - 3)
        ModuleName(n.split("\\."): _*)
      }
      val s = io.Source.fromFile(sourceFile)
      moduleName -> (try s.mkString finally s.close())
    }).toMap

  private def sources(directory: File): Set[File] = {
    val (directories, files) = directory.listFiles.toSet.partition(_.isDirectory)
    files.filter{f => Files.getFileExtension(f.toString) == "vl"} ++ directories.flatMap(sources)
  }

  private def parseModules(modules: Map[ModuleName, String]): Vector[ast.Module] =
    modules.toVector.map{ case (n, s) => parse.parse(n, parse.lex(s)) }
}
