package ammonite.ops

import java.nio.ByteBuffer

import ammonite.ops.PushIter.Filtered

/**
  * A much-simplified version of the `Traversable` trait, defined entirely
  * by a `iterate` method. The boolean returned controls whether or not
  * to abort the foreach early
  */
trait PushIter[+T]{
  /**
    * Equivalent to `foreach`, except the lambda returns a Boolean.
    * If `false`, this causes the iteration to abort early.
    */
  def iterate(call: T => Boolean): Unit

  def hasDefiniteSize = false

  // Appends
  def ++[B >: T](xs: PushIter[B]) = new PushIter.Append[B](this, xs)
  def :+[B >: T](xs: B) = new PushIter.Append[B](this, PushIter(xs))
  def ++:[B >: T](xs: PushIter[B]) = new PushIter.Append[B](xs, this)
  def +:[B >: T](xs: B) = new PushIter.Append[B](PushIter(xs), this)

  // Transformations
  def foreach(f: T => Unit) = iterate{ t => f(t); true}
  def map[B](f: T => B) = new PushIter.Mapped(this, f)
  def filter(p: T => Boolean) = new Filtered(this, p)
  def flatMap[B](f: T => PushIter[B]) = new PushIter.FlatMapped(this, f)
  def collect[V](p: PartialFunction[T, V]) = this.filter(p.isDefinedAt).map(p)
  def flatten[B](f: T => PushIter[B]) = ???

  // Splices
  def take(n: Int): PushIter[T] = new PushIter.Take(this, n)
  def drop(n: Int): PushIter[T] = new PushIter.Drop(this, n)
  def slice(from: Int, until: Int) = this.drop(from).take(until - from)

  def takeWhile(p: T => Boolean) = new PushIter.TakeWhile(this, p)
  def dropWhile(p: T => Boolean) = new PushIter.DropWhile(this, p)

  // Terminals
  def foldLeft[B](start: B)(f: (B, T) => B) = {
    var current = start
    this.foreach{x => current = f(current, x) }
    current
  }

  def size = this.foldLeft(0)((l, _) => l + 1)

  // Boolean Predicates
  def forall(f: T => Boolean) = !exists(!f(_))
  def exists(f: T => Boolean) = this.filter(f).size > 0
  def contains[A1 >: T](f: A1) = this.exists(_ == f)

  // Selecting individual items
  def headOption = this.take(1).foldLeft(None: Option[T])((_, x) => Some(x))
  def head = headOption.get
  def lastOption = this.foldLeft(None: Option[T])((_, t) => Some(t))
  def last = lastOption.get
  def find(f: T => Boolean) = this.filter(f).foldLeft(None: Option[T])((_, t) => Some(t))

  // Indexing
  def indexWhere(f: T => Boolean, from: Int) = this.drop(from).dropWhile(f).size
  def indexOf[V >: T](elem: V, from: Int) = this.indexWhere(_ == elem, from)
}
object PushIter{

  def apply[T](xs: T*) = Apply(xs:_*)
  case class Apply[T](xs: T*) extends PushIter[T]{
    def iterate(call: T => Boolean) = xs.takeWhile(call(_))
  }
  class Append[+T](lhs: PushIter[T], rhs: PushIter[T]) extends PushIter[T]{
    def iterate(call: T => Boolean) = {
      lhs.iterate(call)
      rhs.iterate(call)
    }
  }
  class Mapped[+T, +V](lhs: PushIter[T], f: T => V) extends PushIter[V]{
    def iterate(call: V => Boolean) = {
      lhs.iterate(t => call(f(t)))
    }
  }
  class FlatMapped[+T, +V](lhs: PushIter[T], f: T => PushIter[V]) extends PushIter[V]{
    def iterate(call: V => Boolean) = {
      lhs.iterate{ t => f(t).iterate(call); true}
    }
  }
  class Filtered[+T](lhs: PushIter[T], p: T => Boolean) extends PushIter[T]{
    def iterate(call: T => Boolean) = {
      lhs.iterate{ t => if (p(t)) call(t); true}
    }
  }
  class Take[+T](lhs: PushIter[T], n: Int) extends PushIter[T]{
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
  class TakeWhile[+T](lhs: PushIter[T], f: T => Boolean) extends PushIter[T]{
    def iterate(call: T => Boolean) = {
      lhs.iterate{ t => f(t) && call(t) }
    }
  }
  class DropWhile[+T](lhs: PushIter[T], f: T => Boolean) extends PushIter[T]{
    def iterate(call: T => Boolean) = {
      var started = false
      lhs.iterate{ t =>
        if (started) call(t)
        else if (f(t)) true
        else {
          started = true
          call(t)
        }
      }
    }
  }
  class Drop[+T](lhs: PushIter[T], n: Int) extends PushIter[T]{
    def iterate(call: T => Boolean) = {
      var i = 0
      lhs.iterate{ t =>
        if (i >= n) call(t)
        i += 1
        true
      }
    }
  }
  implicit def StreamyToSeq[T](s: PushIter[T]): Seq[T] = {
    val buffer = collection.mutable.Buffer.empty[T]
    s.iterate{ t => buffer.append(t); true}
    buffer
  }
  class InputStreamChunks(is: java.io.InputStream, n: Int) extends PushIter[Array[Byte]]{
    def iterate(call: Array[Byte] => Boolean) = {
      val out = new java.io.ByteArrayOutputStream()
      var r = 0
      while (r != -1) {
        val buffer = new Array[Byte](n)
        r = is.read(buffer)
        val truncated =
          if(r != n) buffer.take(r)
          else buffer

        if (!call(truncated)) r = -1 // exit loop
      }
      is.close()
    }
  }
  class Lines(is: java.io.InputStream) extends PushIter[String]{
    def iterate(call: String => Boolean) = {
      val chunkStreamy = new InputStreamChunks(is, 8192)
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
