package com.github.agetakoyaki29.scala.geometry.dim2

import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry._


object Dim2 extends Dim2Factory[Dim2] {
  def apply(x: Double, y: Double): Dim2 = new Dim2(x, y)
}


class Dim2(_x: Double, _y: Double) extends IndexedSeq[Double] with Dim {
  val (x, y) = (_x, _y)

  val factory: Dim2Factory[_ <: Dim2] = Dim2

  // -- validate --

  validate apply this
  this foreach {validateEach apply _}

  protected def validate: Dim2 => Dim2 = Dim2.Identity
  protected def validateEach: Double => Double = MyDouble.NotNaN andThen MyDouble.NotInfinite

  // ---- for IndexedSeq ----

  override final def foreach[U](f: Double => U): Unit = {f(x); f(y)}
  final def apply(idx: Int): Double = idx match { case 0 => x case 1 => y }
  final def length: Int = 2

  // ---- from IndexedSeq ----

  @UpRet def reverseD2: Dim2 = factory(super.reverse)

  @UpRet def updatedD2(idx: Int, elem: Double): Dim2 = factory(super.updated(idx, elem))

  @UpRet def mapD2(f: Double => Double): Dim2 = factory(super.map(f))

  @UpRet def zipmapD2(op: Dim2)(f: (Double, Double) => Double): Dim2 = factory(this zip op map f.tupled)

  // ---- basic operators ----

  @UpRet def abs: Dim2 = mapD2{_.abs}

  @UpRet def unary_+(): Dim2 = factory(this)
  @UpRet def unary_-(): Dim2 = mapD2{-_}

  @UpRet def +(op: Dim2): Dim2 = zipmapD2(op) {_+_}

  @UpRet def -(op: Dim2): Dim2 = zipmapD2(op) {_-_}

  @UpRet def *(d: Double): Dim2 = this mapD2 {_*d}

  /**
   * @param d NotZero(for zero / zero = NaN, any / zero = inf)
   */
  @UpRet def /(d: Double): Dim2 = {
    MyDouble.NotZero(d)
    this mapD2 {_/d}
  }

  @UpRet def minus: Dim2 = -this

  /**
   * return 1 norm, same direction Dim2
   * isZero => throw IllegalStateException
   */
  @UpRet def normalized: Dim2 = {
    if(this.isZero) throw new IllegalStateException("Zero can't normalize")
    this / norm
  }

  // ---- std ----

  override def equals(op: Any) = op match {
    case dim2: Dim2 => this zip dim2 forall tupled{_==_}
    case _ => false
  }

  override def hashCode: Int = 32*x.## + y.##

}
