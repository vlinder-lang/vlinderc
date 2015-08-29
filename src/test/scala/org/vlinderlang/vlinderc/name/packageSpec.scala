package org.vlinderlang.vlinderc.name

import org.scalatest.FlatSpec
import org.vlinderlang.vlinderc.ModuleName
import org.vlinderlang.vlinderc.parse.{lex, parse}

class packageSpec extends FlatSpec {
  "resolve" should "work" in {
    val vlinder_logCode = """
      typealias Record = ()

      typealias Logger = (Record) => ()

      sub info(logger: Logger, message: __String): () { }
    """
    val mainCode = """
      import vlinder.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
      }
    """
    val vlinder_log = parse(ModuleName("vlinder", "log"), lex(vlinder_logCode))
    val main = parse(ModuleName("main"), lex(mainCode))
    resolve(Vector(vlinder_log, main))
  }
}
