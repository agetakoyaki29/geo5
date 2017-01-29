package com.github.agetakoyaki29.scala.geometry

import org.scalatest.matchers.{Matcher, MatchResult}

import Delta._


trait DeltaMatchers {

  def =~(right: Double) = Matcher { (left: Double) => MatchResult(
    left =~ right,
    s"$left did not equal $right in ${implicitly[Double]}",   // TODO fix in ~~~
    s"$left equal $right in ${implicitly[Double]}"
  ) }

}
