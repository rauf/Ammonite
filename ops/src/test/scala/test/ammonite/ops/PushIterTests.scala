package test.ammonite.ops

import java.io.ByteArrayInputStream

import ammonite.ops._
import utest._
import utest.framework.TestSuite

import scala.collection.mutable

object PushIterTests extends TestSuite{

  /**
    * Wraps a simple PushIter that operates on an input stream created from a
    * string, and records when the input stream is closed.
    */
  class TestSource(val string: String) {
    var closed = false
    val inputStream = new ByteArrayInputStream(string.getBytes){
      override def close() = {
        assert(!closed)
        closed = true
      }
    }
    val pushIter = new PushIter.InputStreamChunks(inputStream, 3 /* arbitrary */)
  }

  /**
    * Verifies that a PushIter behaves as expected: given some input strings,
    * wrap them in TestSources and check that when you combine the sources'
    * PushIters using `combineIter`, the output is the same as when you combine
    * the grouped chunks of each string using `combineStrChunks`
    */
  def check(strings: Seq[String],
            combineIter: Seq[PushIter[Array[Byte]]] => PushIter[Array[Byte]],
            combineStrChunks: Seq[Seq[String]] => Seq[String]) = {
    val testSources = strings.map(new TestSource(_))
    val output = mutable.Buffer.empty[Byte]
    val combinedIter = combineIter(testSources.map(_.pushIter))
    val expectedOutput = combineStrChunks(strings.map(_.grouped(3).toVector)).mkString

    // Make sure that all the sources were not closed before we iterate, and
    // that they *are* closed after we iterate on the combined PushIter
    assert(testSources.forall(!_.closed))
    combinedIter.foreach(output.appendAll)
    assert(testSources.forall(_.closed))

    // Make sure the output of the combined iter, as a String,
    // is the same as the combined string
    val outputString = new String(output.toArray)
    assert(outputString == expectedOutput)
    outputString
  }

  val tests = TestSuite {
    val testString = "Hello World I am a Cow"
    'noOp - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter},
      {case Seq(strs) => strs}
    )
    'map - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter.map(x => x ++ x)},
      {case Seq(strs) => strs.map(x => x ++ x)}
    )
    'filter - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter1) => iter1.filter(!_.contains(' '))},
      {case Seq(strs1) => strs1.filter(!_.contains(' '))}
    )
    'flatMap - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter1) =>
        iter1.flatMap(x =>
          if (x == "Hel".getBytes) PushIter("Hel".getBytes, "lll".getBytes) else PushIter(x)
        )
      },
      {case Seq(strs1) =>
        strs1.flatMap(x =>
          if (x == "Hel".getBytes) PushIter("Hel", "lll") else PushIter(x)
        )
      }
    )
    'take - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter.take(2)},
      {case Seq(strs) => strs.take(2) }
    )

    'takeWhile - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter.takeWhile(!_.contains('W'.toByte))},
      {case Seq(strs) => strs.takeWhile(!_.contains('W'.toByte))}
    )
    'dropWhile - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter.dropWhile(!_.contains('W'.toByte))},
      {case Seq(strs) => strs.dropWhile(!_.contains('W'.toByte))}
    )

    'drop - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter.drop(2)},
      {case Seq(strs) => strs.drop(2) }
    )

    'slice - check(
      Seq("Hello World I am a Cow"),
      {case Seq(iter) => iter.slice(1, 2)},
      {case Seq(strs) => strs.slice(1, 2) }
    )
    'append - check(
      Seq("Hello World ", "I am a Cow"),
      {case Seq(iter1, iter2) => iter1 ++ iter2},
      {case Seq(strs1, strs2) => strs1 ++ strs2}
    )

  }
}
