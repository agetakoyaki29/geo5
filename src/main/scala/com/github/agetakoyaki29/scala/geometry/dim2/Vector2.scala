package com.github.agetakoyaki29.scala.geometry.dim2

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.geometry._


object Vector2 extends Dim2Factory[Vector2] {
  def apply(x: Double, y: Double): Vector2 = new Vector2(x, y)
}


@SameRet
class Vector2(_x: Double, _y: Double) extends Dim2(_x, _y) {

  override val factory: Dim2Factory[_ <: Vector2] = Vector2

  // ----

  /**
   * return dot
   */
  final def dot(op: Vector2): Double = this zip op map tupled{_*_} sum

  final def dotEq0(op: Vector2): Boolean = x*op.x =~ - y*op.y
  final def dotGt0(op: Vector2): Boolean = x*op.x >~ - y*op.y
  final def dotLt0(op: Vector2): Boolean = x*op.x <~ - y*op.y

  /**
   * return No 3 elem of cross
   * Vector3(this) cross Vector3(op) apply 2
   */
  final def cross(op: Vector2): Double = x*op.y - y*op.x

  final def crossEq0(op: Vector2): Boolean = x*op.y =~ y*op.x
  final def crossGt0(op: Vector2): Boolean = x*op.y >~ y*op.x
  final def crossLt0(op: Vector2): Boolean = x*op.y <~ y*op.x

  // ---- instead of SameRet ----

  override def reverseD2 = factory(super.reverseD2)
  override def updatedD2(idx: Int, elem: Double) = factory(super.updatedD2(idx, elem))
  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmapD2(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmapD2(op)(f))
  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+())
  override def unary_-() = factory(super.unary_-())
  override def +(op: Dim2) = factory(super.+(op))
  override def -(op: Dim2) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))
  override def minus = factory(super.minus)
  override def normalized = factory(super.normalized)

}
