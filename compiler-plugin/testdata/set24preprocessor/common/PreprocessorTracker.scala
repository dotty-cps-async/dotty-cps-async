package cps.preprocessor

/**
 * Runtime tracker for preprocessor tests.
 */
object PreprocessorTracker:
  @volatile var wasCalled: Boolean = false
  @volatile var wrapCount: Int = 0

  def reset(): Unit =
    wasCalled = false
    wrapCount = 0

  def markCalled(): Unit =
    wasCalled = true

  def wrap[T](value: T): T =
    wrapCount += 1
    value
