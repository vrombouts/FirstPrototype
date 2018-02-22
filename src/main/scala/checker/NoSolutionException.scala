package checker

final case class NoSolutionException(private val message: String = "The problem posed has no solution")
  extends RuntimeException(message)