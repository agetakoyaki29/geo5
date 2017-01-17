package com.github.agetakoyaki29.scala.geometry.dim2.point

import org.scalatest.matchers.{Matcher, MatchResult}


trait Figure2Matchers {
  def through(right: Point2) = Matcher { (left: Figure2) => MatchResult(
      left through right,
      s"$left not through $right",
      s"$left through $right"
    ) }
  def contain(right: Point2) = Matcher { (left: Figure2) => MatchResult(
      left containPoint2 right,
      s"$left not contain $right",
      s"$left contain $right"
    ) }

  def same(right: Figure2) = Matcher { (left: Figure2) => MatchResult(
      left same right,
      s"$left not same $right",
      s"$left same $right"
    ) }
  def contain(right: Figure2) = Matcher { (left: Figure2) => MatchResult(
      left contain right,
      s"$left not contain $right",
      s"$left contain $right"
    ) }
  def isIntersect(right: Figure2) = Matcher { (left: Figure2) => MatchResult(
      left isIntersect right,
      s"$left is not intersect $right",
      s"$left is intersect $right"
    ) }

  def isNotIntersect(right: Figure2) = Matcher { (left: Figure2) => MatchResult(
      !(left isIntersect right),
      s"$left is intersect $right",
      s"$left is not intersect $right"
    ) }

}
