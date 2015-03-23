package com.msci.playground

/**
 * Created by cry on 2015.01.28..
 */

//case class Timestamped(fs: FollowerStats, ts: Long)
//
//case class FollowerStats()
//
//case class Cache(hits: Int, misses: Int) {
//  def get(u: String): Option[Timestamped] = ???
//
//  def update(u: String, fs: Timestamped): Cache = ???
//}
//
//trait State[S, +A] {
//  def run: S => (S, A)
//
//  def map[B](f: A => B): State[S, B] = State {s =>
//    val (s1, a) = run(s)
//    (s1, f(a))
//  }
//
//  def flatMap[B](f: A => State[S, B]): State[S, B] = State {s =>
//    val (s1, a) = run(s)
//    f(a).run(s1)
//  }
//}
//
//object State {
//  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
//    def run = f
//  }
//}
//
//object StateMonadPlayground extends App {
//
//  def folowerStatsS(u: String): State[Cache, FollowerStats] = {
//    for {
//      ofs <- checkCacheS(u)
//      fs <- ofs match {
//        case Some(fs) => State { s => (s, fs)}
//        case None => retrieveS(u)
//      }
//    } yield fs
//  }
//
//  def folowerStats(u: String): Cache => (Cache, FollowerStats) = { c =>
//    val (c1, ofs) = checkCache(u)(c)
//    ofs match {
//      case Some(fs) => (c1, fs)
//      case None => retrieve(u)(c1)
//    }
//  }
//
//  def checkCacheS(u: String): State[Cache, Option[FollowerStats]] = {
//    State { c: Cache => c.get(u) match {
//      case Some(Timestamped(fs, ts)) if (!stale(ts)) =>
//        (c.copy(hits = c.hits + 1), Some(fs))
//      case _ =>
//        (c.copy(misses = c.misses + 1), None)
//    }
//    }
//  }
//
//  def checkCache(u: String): Cache => (Cache, Option[FollowerStats]) = { c =>
//    c.get(u) match {
//      case Some(Timestamped(fs, ts)) if (!stale(ts)) =>
//        (c.copy(hits = c.hits + 1), Some(fs))
//      case _ =>
//        (c.copy(misses = c.misses + 1), None)
//    }
//  }
//
//  def stale(ts: Long): Boolean = {
//    System.currentTimeMillis() - ts > (5 * 60 * 1000L)
//  }
//
//  def retrieve(u: String): Cache => (Cache, FollowerStats) = { c =>
//    val fs = FollowerStats()
//    val tfs = Timestamped(fs, System.currentTimeMillis())
//    (c.update(u, tfs), fs)
//  }
//
//  def retrieveS(u: String): State[Cache, FollowerStats] = State{ c =>
//    val fs = FollowerStats()
//    val tfs = Timestamped(fs, System.currentTimeMillis())
//    (c.update(u, tfs), fs)
//  }
//
//}