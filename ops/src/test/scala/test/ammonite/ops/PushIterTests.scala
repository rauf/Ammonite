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
    check0[PushIter, Seq, Array[Byte], String](
      strings,
      combineIter,
      combineStrChunks,
      _.toArray.flatten,
      _.mkString
    )
  }
  def checkSelect(strings: Seq[String],
            combineIter: Seq[PushIter[Array[Byte]]] => Array[Byte],
            combineStrChunks: Seq[Seq[String]] => String) = {
    type Id[T] = T
    check0[Id, Id, Array[Byte], String](
      strings,
      combineIter,
      combineStrChunks,
      x => x,
      x => x
    )
  }

  def checkInt[T, V](strings: Seq[String],
                     combineIter: Seq[PushIter[Array[Byte]]] => Array[Byte],
                     combineStrChunks: Seq[Seq[String]] => String) = {
    type Id[T] = T
    check0[Id, Id, T, V](
      strings,
      combineIter,
      combineStrChunks,
      x => x,
      x => x
    )
  }

  def check0[M[_], N[_], T, V](strings: Seq[String],
                         combineIter: Seq[PushIter[Array[Byte]]] => M[Array[Byte]],
                         combineStrChunks: Seq[Seq[String]] => N[String],
                         combineIter2: M[Array[Byte]] => T,
                         combineStrChunks2: N[String] => V
                        ) = {
    val testSources = strings.map(new TestSource(_))
    val iters = testSources.map(_.pushIter)

    val expectedOutput = combineStrChunks2(combineStrChunks(strings.map(_.grouped(3).toVector)))

    // Make sure that all the sources were not closed before we iterate, and
    // that they *are* closed after we iterate on the combined PushIter
    assert(testSources.forall(!_.closed))
    val combinedIter = combineIter(iters)
    val output = combineIter2(combinedIter)
    assert(testSources.forall(_.closed))

    // Make sure the output of the combined iter, as a String,
    // is the same as the combined string
    val outputString = new String(output)
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
    'Appends{
      '++ - check(
        Seq("Hello World ", "I am a Cow"),
        {case Seq(iter1, iter2) => iter1 ++ iter2},
        {case Seq(strs1, strs2) => strs1 ++ strs2}
      )
      ':+ - check(
        Seq("Hello World "),
        {case Seq(iter1) => iter1 :+ "mooo".getBytes},
        {case Seq(strs1) => strs1 :+ "mooo"}
      )

      '++: - check(
        Seq("Hello World ", "I am a Cow"),
        {case Seq(iter1, iter2) => iter1 ++: iter2},
        {case Seq(strs1, strs2) => strs1 ++: strs2}
      )
      '+: - check(
        Seq("Hello World "),
        {case Seq(iter1) => "mooo".getBytes +: iter1},
        {case Seq(strs1) => "mooo" +: strs1}
      )
    }
    'Transformations{

      'foreach - {

      }
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

      'collect - check(
        Seq("Hello World I am a Cow"),
        {case Seq(iter1) => iter1.collect{case x if x.contains(' '.toByte) => x}},
        {case Seq(strs1) => strs1.collect{case x if x.contains(' '.toByte) => x}}
      )

      'flatten - check(
        Seq("Hello World I am a Cow"),
        {case Seq(iter1) => iter1.filter(!_.contains(' '))},
        {case Seq(strs1) => strs1.filter(!_.contains(' '))}
      )
    }

    'Splices {
      'take - check(
        Seq("Hello World I am a Cow"),
        {case Seq(iter) => iter.take(2)},
        {case Seq(strs) => strs.take(2) }
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
    }

    'Terminals{
      'foldLeft - checkSelect(
        Seq("Hello World I am a Cow"),
        {case Seq(iter) => iter.foldLeft("".getBytes)((base, chunk) => chunk ++ base)},
        {case Seq(strs) => strs.foldLeft("")((base, chunk) => chunk ++ base)}
      )
//      'size - checkSelect(
//        Seq("Hello World I am a Cow"),
//        {case Seq(iter) => iter.size},
//        {case Seq(strs) => strs.size}
//      )

    }
  }
}
