package freechips.rocketchip.pfa

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{TwoWayCounter, UIntIsOneOf, DecoupledHelper}
import freechips.rocketchip.pfa._

class EvictIO extends Bundle {
  // requests are pfns and they are handled in PFAEvictPath
  val req = Decoupled(UInt(64.W))
  // high when a req has completed
  val resp = Flipped(Decoupled(Bool()))
}

class NewPageRequest extends Bundle {
  val reserved = UInt(24.W)
  val pageid = UInt(28.W)
  val vaddr = UInt(39.W)
}

class NewpageIO extends Bundle {
  val req = Decoupled(new NewPageRequest)
}

class PFARequest extends Bundle {
  val reserved = UInt(24.W)
  val pageid = UInt(28.W)
  val protbits = UInt(10.W)
  val faultvpn = UInt(27.W)
  val pteppn = UInt(54.W)
}

class PFAIO extends Bundle {
  // the ptw drives the requests
  val req = Decoupled(new PFARequest)
  val resp = Flipped(Decoupled(UInt(64.W))) // pfa's replies TODO: whats this for?
  val fpq_avail = Input(Bool())
}

case class PFAControllerParams(addr: BigInt, beatBytes: Int)

class PFAClientBundle extends Bundle {
  val paddr = UInt(64.W)
  val pageid = UInt(64.W)
  val dstmac = UInt(48.W)
}

class PFARemoteMemClient(name: String, val clientAddr: BigInt, val write: Boolean)
    (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(name, IdRange(0, 1))

  lazy val module = new PFARemoteMemClientModule(this)
}

class PFARemoteMemClientModule(outer: PFARemoteMemClient)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new PFAClientBundle))
    val comp = Decoupled(Bool())
  })

  val clientAddr = outer.clientAddr

  val req = Reg(new PFAClientBundle)
  // PAGE_READ = 0, PAGE_WRITE = 1
  val opcode = if (outer.write) 1.U(16.W) else 0.U(16.W)
  val rmemXact = Reg(UInt(32.W))

  val (tl, edge) = outer.node.out(0)
  val (s_idle :: s_client_set_acq :: s_client_set_gnt ::
       s_client_nreq_acq :: s_client_nreq_gnt ::
       s_client_req_acq  :: s_client_req_gnt  ::
       s_client_nresp_acq :: s_client_nresp_gnt ::
       s_client_resp_acq  :: s_client_resp_gnt :: s_comp :: Nil) = Enum(12)
  val state = RegInit(s_idle)

  // 0x00 - src address (from free queue)
  // 0x08 - dst address (from free queue)
  // 0x10 - opcode + remote mac address
  // 0x18 - remote pageno
  val setAddrs = VecInit(
    Seq((if (outer.write) 0x00 else 0x08), 0x10, 0x18)
      .map(off => (clientAddr + off).U))
  val setData = VecInit(Seq(
    req.paddr, Cat(opcode, req.dstmac), req.pageid))
  val (setIdx, setDone) = Counter(state === s_client_set_gnt && tl.d.valid, 3)

  val setAcq = edge.Put(
    fromSource = 0.U,
    toAddress = setAddrs(setIdx),
    lgSize = 3.U,
    data = setData(setIdx))._2

  val getAddr = MuxLookup(state, 0.U,
    Seq((s_client_nreq_acq, 0x28), (s_client_req_acq, 0x20),
        (s_client_nresp_acq, 0x2C), (s_client_resp_acq, 0x24)).map {
          case (s, off) => (s -> (clientAddr + off).U) })

  val getAcq = edge.Get(
    fromSource = 0.U,
    toAddress = getAddr,
    lgSize = 2.U)._2

  io.req.ready := state === s_idle
  io.comp.valid := state === s_comp
  io.comp.bits := DontCare

  tl.a.valid := state.isOneOf(
    s_client_set_acq, s_client_nreq_acq, s_client_req_acq,
    s_client_nresp_acq, s_client_resp_acq)
  tl.a.bits := Mux(state === s_client_set_acq, setAcq, getAcq)
  tl.d.ready := state.isOneOf(
    s_client_set_gnt, s_client_nreq_gnt, s_client_req_gnt,
    s_client_nresp_gnt, s_client_resp_gnt)

  when (io.req.fire()) {
    req := io.req.bits
    state := s_client_set_acq
  }

  when (tl.a.fire()) {
    state := MuxLookup(state, state, Seq(
      s_client_set_acq -> s_client_set_gnt,
      s_client_nreq_acq -> s_client_nreq_gnt,
      s_client_req_acq  -> s_client_req_gnt,
      s_client_nresp_acq -> s_client_nresp_gnt,
      s_client_resp_acq -> s_client_resp_gnt))
  }

  when (tl.d.fire()) {
    val isZero = tl.d.bits.data === 0.U
    switch (state) {
      is (s_client_set_gnt) {
        state := Mux(setDone, s_client_nreq_acq, s_client_set_acq)
      }
      is (s_client_nreq_gnt) {
        state := Mux(isZero, s_client_nreq_acq, s_client_req_acq)
      }
      is (s_client_req_gnt) {
        rmemXact := tl.d.bits.data
        state := s_client_nresp_acq
      }
      is (s_client_nresp_gnt) {
        state := Mux(isZero, s_client_nresp_acq, s_client_resp_acq)
      }
      is (s_client_resp_gnt) {
        assert(tl.d.bits.data === rmemXact, "PFAFetchPath: ID does not match")
        state := s_comp
      }
    }
  }

  when (io.comp.fire()) { state := s_idle }
}

class PFAFetchPath(clientAddr: BigInt)(implicit p: Parameters) extends LazyModule {
  val client = LazyModule(new PFARemoteMemClient(
    "pfa-fetch-rmem", clientAddr, false))
  val writenode = TLHelper.makeClientNode(
    "pfa-fetch-modpte", IdRange(0, 1))

  val node = TLIdentityNode()
  node := client.node
  node := writenode

  lazy val module = new PFAFetchPathModule(this)
}

class PFAFetchPathModule(outer: PFAFetchPath) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val fetch = Flipped(new PFAIO)
    val free = Flipped(Decoupled(UInt(64.W)))
    val newpages = new NewpageIO
    val inprog = Output(Bool())
    val evictinprog = Input(Bool())
    val dstmac = Input(UInt(48.W))
  })

  val client = outer.client.module
  val (tl, edge) = outer.writenode.out(0)

  val (s_idle :: s_request :: s_comp :: Nil) = Enum(3)
  val state = RegInit(s_idle)

  val (modpte_idle :: modpte_acq :: modpte_gnt :: Nil) = Enum(3)
  val modpte = RegInit(modpte_idle)

  val reserved = Reg(UInt(24.W))
  val pageid = Reg(UInt(28.W))
  val protbits = Reg(UInt(10.W))
  val pteppn = Reg(UInt(54.W))
  val faultvpn = Reg(UInt(27.W))
  val paddr = Reg(UInt(64.W))
  val newpte = Cat(paddr(63, 12), protbits)

  val canTakeReq = (state === s_idle) && !io.evictinprog
  io.fetch.req.ready := io.free.valid && canTakeReq
  io.free.ready := io.fetch.req.valid && canTakeReq
  io.fetch.fpq_avail := io.free.valid
  io.inprog := state =/= s_idle

  val compHelper = DecoupledHelper(
    io.fetch.resp.ready,
    io.newpages.req.ready,
    client.io.comp.valid)
  val canComp = state === s_comp && modpte === modpte_idle

  client.io.req.valid := state === s_request
  client.io.req.bits.paddr := paddr
  client.io.req.bits.dstmac := io.dstmac
  client.io.req.bits.pageid := pageid
  client.io.comp.ready := compHelper.fire(client.io.comp.valid, canComp)

  io.fetch.resp.valid := compHelper.fire(io.fetch.resp.ready, canComp)
  io.fetch.resp.bits := newpte

  io.newpages.req.valid := compHelper.fire(io.newpages.req.ready, canComp)
  io.newpages.req.bits.reserved := reserved
  io.newpages.req.bits.pageid := pageid
  io.newpages.req.bits.vaddr := faultvpn << 12.U

  tl.a.valid := modpte === modpte_acq
  tl.a.bits := edge.Put(
    fromSource = 0.U,
    toAddress = pteppn,
    lgSize = 3.U,
    data = newpte)._2
  tl.d.ready := modpte === modpte_gnt

  when (io.fetch.req.fire()) {
    reserved := io.fetch.req.bits.reserved
    pageid := io.fetch.req.bits.pageid
    protbits := io.fetch.req.bits.protbits
    pteppn := io.fetch.req.bits.pteppn
    faultvpn := io.fetch.req.bits.faultvpn
    state := s_request
  }

  when (io.free.fire()) {
    paddr := io.free.bits
    modpte := modpte_acq
  }

  when (client.io.req.fire()) { state := s_comp }

  when (tl.a.fire()) { modpte := modpte_gnt }

  when (tl.d.fire()) { modpte := modpte_idle }

  when (io.fetch.resp.fire()) { state := s_idle }
}

class PFAEvictPath(clientAddr: BigInt)(implicit p: Parameters) extends LazyModule {
  val client = LazyModule(new PFARemoteMemClient(
    "pfa-evict-path", clientAddr, true))
  val node = client.node
  lazy val module = new PFAEvictPathModule(this)
}

class PFAEvictPathModule(outer: PFAEvictPath) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val evict = Flipped(new EvictIO)
    val dstmac = Input(UInt(48.W))
    val inprog = Output(Bool())
  })

  val client = outer.client.module
  client.io.req.valid := io.evict.req.valid
  client.io.req.bits.paddr := Cat(io.evict.req.bits(35, 0), 0.U(12.W))
  client.io.req.bits.pageid := io.evict.req.bits(63, 36)
  client.io.req.bits.dstmac := io.dstmac
  io.evict.req.ready := client.io.req.ready
  io.evict.resp <> client.io.comp

  io.inprog := !client.io.req.ready || client.io.comp.valid
}

trait PFAControllerBundle extends Bundle {
  val evict = new EvictIO
  val newpages = Flipped(new NewpageIO)
  val free = Decoupled(UInt(64.W))
  val fetchinprog = Input(Bool())
  val dstmac = Output(UInt(64.W))
}

trait PFAControllerModule extends HasRegMap {
  val io: PFAControllerBundle
  val qDepth = 64

  val evictQueue = Module(new Queue(UInt(64.W), qDepth))
  val evictsInProg = TwoWayCounter(io.evict.req.fire(), io.evict.resp.fire(), qDepth)
  val evictStat = Wire(init = (0.U(64.W)))
  io.evict.req <> evictQueue.io.deq
  io.evict.resp.ready := true.B // control always ready for eviction resp
  evictStat := Mux(io.fetchinprog, qDepth.U,
                   Mux(evictsInProg > evictQueue.io.count, evictsInProg, evictQueue.io.count))

  val freeQueue = Module(new Queue(UInt(64.W), qDepth))
  io.free <> freeQueue.io.deq

  val newPageidQueue = Module(new Queue(UInt(52.W), qDepth))
  val newVaddrQueue = Module(new Queue(UInt(39.W), qDepth))
  newPageidQueue.io.enq.valid := io.newpages.req.valid
  newVaddrQueue.io.enq.valid := io.newpages.req.valid
  io.newpages.req.ready := newPageidQueue.io.enq.ready && newVaddrQueue.io.enq.ready
  newPageidQueue.io.enq.bits := Cat(io.newpages.req.bits.reserved, io.newpages.req.bits.pageid)
  newVaddrQueue.io.enq.bits := io.newpages.req.bits.vaddr

  val dstmac = Reg(UInt(48.W))
  io.dstmac := dstmac

  regmap(
    0 -> Seq(RegField.w(64, freeQueue.io.enq)),              // free queue
    8 -> Seq(RegField.r(64, qDepth.U - freeQueue.io.count)), // free stat
    16 -> Seq(RegField.w(64, evictQueue.io.enq)),            // evict queue
    24 -> Seq(RegField.r(64, qDepth.U - evictStat)),         // evict stat
    32 -> Seq(RegField.r(64, newPageidQueue.io.deq)),        // new pageid queue
    40 -> Seq(RegField.r(64, newVaddrQueue.io.deq)),         // new vaddr queue
    48 -> Seq(RegField.r(64, newPageidQueue.io.count)),      // new stat
    56 -> Seq(RegField(48, dstmac)))          // remote MAC
}

class PFAController(c: PFAControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(c.addr, "pfa",  Seq("ucbbar,pfa"),
                           beatBytes = c.beatBytes)(
                            new TLRegBundle(c, _) with PFAControllerBundle)(
                            new TLRegModule(c, _, _) with PFAControllerModule)

class PFA(addr: BigInt, clientAddr: BigInt, beatBytes: Int = 8)(implicit p: Parameters)
    extends LazyModule {
  val control = LazyModule(new PFAController(
      PFAControllerParams(addr, beatBytes)))
  val fetchPath = LazyModule(new PFAFetchPath(clientAddr))
  val evictPath = LazyModule(new PFAEvictPath(clientAddr))

  val mmionode = TLIdentityNode()
  val dmanode = TLIdentityNode()

  control.node := mmionode
  dmanode :=* fetchPath.node
  dmanode :=* evictPath.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val remoteFault = Flipped(new PFAIO)
    })

    evictPath.module.io.evict <> control.module.io.evict
    evictPath.module.io.dstmac := control.module.io.dstmac
    fetchPath.module.io.evictinprog := evictPath.module.io.inprog

    io.remoteFault <> fetchPath.module.io.fetch
    fetchPath.module.io.free <> control.module.io.free
    fetchPath.module.io.dstmac := control.module.io.dstmac
    control.module.io.newpages <> fetchPath.module.io.newpages
    control.module.io.fetchinprog := fetchPath.module.io.inprog
  }
}

trait HasPeripheryPFA { this: BaseSubsystem =>

  private val clientAddr = BigInt(0x10018000)
  private val pfaAddr = BigInt(0x10017000)
  private val portName = "PFA"

  val pfa = LazyModule(new PFA(pfaAddr, clientAddr, sbus.beatBytes))
  sbus.toVariableWidthSlave(Some(portName)) { pfa.mmionode }
  sbus.fromPort(Some(portName))() :=* pfa.dmanode
}
