package org.vlinderlang.vlinderc

sealed abstract class DiagnosticKind
case object Warning extends DiagnosticKind
case object Error extends DiagnosticKind

trait Diagnostic {
  def kind: DiagnosticKind
  def message: String

  final def format: String = {
    val kindString = kind match {
      case Warning => s"warning"
      case Error => s"error"
    }
    s"$kindString: $message"
  }
}
