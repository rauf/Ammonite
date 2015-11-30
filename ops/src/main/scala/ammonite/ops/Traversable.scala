package ammonite.ops

import java.io.InputStream
import java.nio.ByteBuffer

import ammonite.ops.Streamy.Filtered

/**
  * A much-simplified version of the `Traversable` trait, defined entirely
  * by a `doForeach` method. The boolean returned controls whether or not
  * to abort the foreach early
  */
trait Streamy[+T]{

  def iterate(call: T => Boolean): Unit

  def foreach(f: T => Unit) = iterate{ t => f(t); true}
  def size = {

  }
  def hasDefiniteSize = false
  def ++[B >: T](xs: Streamy[B]) = new Streamy.Append[B](this, xs)
  def map[B](f: T => B) = new Streamy.Mapped(this, f)
  def flatMap[B](f: T => Streamy[B]) = new Streamy.FlatMapped(this, f)
  def filter(p: T => Boolean) = new Filtered(this, p)
  def take(n: Int): Streamy[T] = new Streamy.Take(this, n)
  def drop(n: Int): Streamy[T] = new Streamy.Drop(this, n)
  def slice(from: Int, until: Int) = this.drop(from).take(until)
//  def takeWhile(p: T => Boolean) = new Streamy(() => func().takeWhile(p) -> close)
//  def dropWhile(p: T => Boolean) = new Streamy(() => func().dropWhile(p) -> close)

}
object Streamy{
  class Append[+T](lhs: Streamy[T], rhs: Streamy[T]) extends Streamy[T]{
    def iterate(call: T => Boolean) = {
      lhs.iterate(call)
      rhs.iterate(call)
    }
  }
  class Mapped[+T, +V](lhs: Streamy[T], f: T => V) extends Streamy[V]{
    def iterate(call: V => Boolean) = {
      lhs.iterate(t => call(f(t)))
    }
  }
  class FlatMapped[+T, +V](lhs: Streamy[T], f: T => Streamy[V]) extends Streamy[V]{
    def iterate(call: V => Boolean) = {
      lhs.iterate{ t => f(t).iterate(call); true}
    }
  }
  class Filtered[+T](lhs: Streamy[T], p: T => Boolean) extends Streamy[T]{
    def iterate(call: T => Boolean) = {
      lhs.iterate{ t => if (p(t)) call(t); true}
    }
  }
  class Take[+T](lhs: Streamy[T], n: Int) extends Streamy[T]{
    def iterate(call: T => Boolean) = {
      var i = 0
      lhs.iterate{ t =>
        if (i < n) {
          call(t)
          i += 1
          true
        }else false
      }
    }
  }
  class Drop[+T](lhs: Streamy[T], n: Int) extends Streamy[T]{
    def iterate(call: T => Boolean) = {
      var i = 0
      iterate{ t =>
        if (i >= n) call(t)
        i += 1
        true
      }
    }
  }
  implicit def StreamyToSeq[T](s: Streamy[T]): Seq[T] = {
    val buffer = collection.mutable.Buffer.empty[T]
    s.iterate{ t => buffer.append(t); true}
    buffer
  }
  class InputStreamChunkStreamy(is: InputStream, n: Int) extends Streamy[Array[Byte]]{
    def iterate(call: Array[Byte] => Boolean) = {
      val out = new java.io.ByteArrayOutputStream()
      var r = 0
      while (r != -1) {
        val buffer = new Array[Byte](n)
        r = is.read(buffer)
        val truncated =
          if(r != n) buffer.take(r)
          else buffer
        call(truncated)
      }
      is.close()
    }
  }
  class LineStreamy(is: InputStream) extends Streamy[String]{
    def iterate(call: String => Boolean) = {
      val chunkStreamy = new InputStreamChunkStreamy(is, 8192)
      val byteBuffer = ByteBuffer.allocate(0)
      val chunks = collection.mutable.Buffer.empty[String]
      chunkStreamy.iterate{chunk =>
        var continue = true
        for((line, i) <- new String(chunk).linesIterator.zipWithIndex.takeWhile(_ => continue)){
          if (i > 0) {
            continue = call(chunks.mkString)
            chunks.clear()
          }
          chunks.append(line)
        }
        continue & call(chunks.mkString)
      }
    }
  }
}
