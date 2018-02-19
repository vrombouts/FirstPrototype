package checker

final case class NoSolutionException(private val message: String = "The problem posed has no solution",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)