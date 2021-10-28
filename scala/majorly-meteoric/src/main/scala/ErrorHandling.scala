object ErrorHandling {
  case class ErrorRecord(`type`: String, fileName: String, reason: String) {
    override def toString: String = s"""${`type`}|$fileName|$reason"""
  }
}
