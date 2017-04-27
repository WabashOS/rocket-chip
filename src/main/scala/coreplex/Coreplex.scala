package coreplex

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.util._
import util._
import rocket._
import diplomacy.LazyModule

trait DirectConnection {
  val tiles: Seq[Tile]
  val uncoreTileIOs: Seq[TileIO]

  val tlBuffering = TileLinkDepths(1,1,2,2,0)
  val ultBuffering = UncachedTileLinkDepths(1,2)

  (tiles zip uncoreTileIOs) foreach { case (tile, uncore) =>
    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, tlBuffering) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, ultBuffering) }
    tile.io.slave.foreach { _ <> TileLinkEnqueuer(uncore.slave.get, 1) }

    tile.io.interrupts <> uncore.interrupts

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
    uncore.rpf_req <> tile.io.rpf_req
    tile.io.rpf_res <> uncore.rpf_res
  }
}

trait RemotePageFault extends LazyModule {
  // Nothing needed here
}

trait RemotePageFaultBundle {
  implicit val p: Parameters
  val rpf_req = Decoupled(Bits(width=64))       // remote page fault request
  val rpf_res = Decoupled(Bits(width=64)).flip  // remote page fault response
}

trait RemotePageFaultModule {
  val io: RemotePageFaultBundle
  val outer: RemotePageFault
  val uncoreTileIOs: Seq[TileIO]

  require(uncoreTileIOs.size == 1)

  uncoreTileIOs.foreach { tile =>
    io.rpf_req <> tile.rpf_req
    tile.rpf_res <> io.rpf_res
  }
}

class DefaultCoreplex(c: CoreplexConfig)(implicit p: Parameters)
    extends BaseCoreplex(c)(p) with RemotePageFault {
  override lazy val module = Module(new DefaultCoreplexModule(c, this, new DefaultCoreplexBundle(c)(p))(p))
}

class DefaultCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters)
    extends BaseCoreplexBundle(c)(p) with RemotePageFaultBundle

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle](
    c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters) extends BaseCoreplexModule(c, l, b)(p)
    with DirectConnection with RemotePageFaultModule

/////

trait TileClockResetBundle {
  val c: CoreplexConfig
  val tcrs = Vec(c.nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait AsyncConnection {
  val io: TileClockResetBundle
  val tiles: Seq[Tile]
  val uncoreTileIOs: Seq[TileIO]

  (tiles, uncoreTileIOs, io.tcrs).zipped foreach { case (tile, uncore, tcr) =>
    tile.clock := tcr.clock
    tile.reset := tcr.reset

    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> AsyncTileLinkFrom(tcr.clock, tcr.reset, t) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> AsyncUTileLinkFrom(tcr.clock, tcr.reset, t) }
    tile.io.slave.foreach { _ <> AsyncUTileLinkTo(tcr.clock, tcr.reset, uncore.slave.get)}

    val ti = tile.io.interrupts
    val ui = uncore.interrupts
    ti.debug := LevelSyncTo(tcr.clock, ui.debug)
    ti.mtip := LevelSyncTo(tcr.clock, ui.mtip)
    ti.msip := LevelSyncTo(tcr.clock, ui.msip)
    ti.meip := LevelSyncTo(tcr.clock, ui.meip)
    ti.seip.foreach { _ := LevelSyncTo(tcr.clock, ui.seip.get) }

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
  }
}

class MultiClockCoreplex(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplex(c)(p) {
  override lazy val module = Module(new MultiClockCoreplexModule(c, this, new MultiClockCoreplexBundle(c)(p))(p))
}

class MultiClockCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplexBundle(c)(p)
    with TileClockResetBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle](
    c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters) extends BaseCoreplexModule(c, l, b)(p)
    with AsyncConnection
