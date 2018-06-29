package freechips.rocketchip.pfa

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

object TLHelper {
  def makeClientNode(name: String, sourceId: IdRange)
                    (implicit valName: ValName): TLClientNode =
    makeClientNode(TLClientParameters(name, sourceId))

  def makeClientNode(params: TLClientParameters)
                    (implicit valName: ValName): TLClientNode =
    TLClientNode(Seq(TLClientPortParameters(Seq(params))))

  def makeManagerNode(beatBytes: Int, params: TLManagerParameters)
                     (implicit valName: ValName): TLManagerNode =
    TLManagerNode(Seq(TLManagerPortParameters(Seq(params), beatBytes)))
}
