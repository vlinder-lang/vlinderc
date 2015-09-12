package org.vlinderlang.vlinderc

import com.google.common.io.Files
import java.io.File
import java.nio.file.{Paths, Files => NIOFiles}
import scala.io

object Main {
  def main(args: Array[String]): Unit =
    if (args.size == 0) {
      usage()
    } else {
      try {
        val inputDirs :+ outputDir = args.toSeq.map(new File(_))
        val moduleSources = readSources(inputDirs.toSet)
        val modules = parseModules(moduleSources)
        name.resolve(modules)
        `type`.analyze(modules)
        val cfgs = ast2ssa.convert(modules)
        modules foreach { module =>
          val yaml = module2yaml.convert(module, cfgs)
          val path = outputDir + "/" + module.name.segments.mkString(".") + ".vlm"
          NIOFiles.write(Paths.get(path), yaml.getBytes("UTF-8"))
        }
      } catch {
        case d: Diagnostic => Console.err.println(d.format)
      }
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
        n = n.substring(0, n.size - ".vl".size)
        ModuleName(n.split("\\."): _*)
      }
      val s = io.Source.fromFile(sourceFile)
      moduleName -> (try s.mkString finally s.close())
    }).toMap

  private def sources(directory: File): Set[File] = {
    if (!directory.exists) {
      println("cannot find " + directory)
      sys.exit(1)
    }
    val (directories, files) = directory.listFiles.toSet.partition(_.isDirectory)
    files.filter{f => Files.getFileExtension(f.toString) == "vl"} ++ directories.flatMap(sources)
  }

  private def parseModules(modules: Map[ModuleName, String]): Vector[ast.Module] =
    modules.toVector.map{ case (n, s) => parse.parse(n, parse.lex(s)) }
}
