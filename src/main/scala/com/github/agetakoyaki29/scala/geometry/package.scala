package com.github.agetakoyaki29.scala

package object geometry {
  implicit def d2mrd(d: Double) = new MyRichDouble(d)

}
