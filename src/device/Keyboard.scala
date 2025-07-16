package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2 = new PS2IO
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

class ps2Chisel extends Module {
	val io = IO(new PS2CtrlIO)
	val data = io.ps2.data

	io.in.pready := false.B
	io.in.pslverr := false.B
	io.in.prdata := 0.U

	val fifo = Reg(Vec(16, UInt(8.W)))
	val fifohead = RegInit(0.U(4.W))
	val fifotail = RegInit(0.U(4.W))
	val buffer = RegInit(0.U(10.W))
	val recv_cnt = RegInit(0.U(4.W))
	val recv_succ = RegInit(false.B)

	val ps2_sync_clk = Reg(UInt(3.W))
	ps2_sync_clk := Cat(ps2_sync_clk(1, 0), io.ps2.clk)
	val sampling = ps2_sync_clk(2) & !ps2_sync_clk(1)

	val s_idle :: s_start :: s_recv :: s_stop :: Nil = Enum(4)
	val state = RegInit(s_idle)
	state := MuxLookUp(state, s_idle)(Seq(
		s_idle -> Mux(sampling & ~data, s_recv, s_idle),
		s_recv -> Mux(sampling && recv_cnt === 8.U, s_stop, s_recv),
		s_stop -> Mux(sampling & data, s_idle, s_stop)
	))
	when(sampling && state === s_idle && ~data) {
		recv_cnt := 0.U
		buffer := 0.U
		recv_succ := false.B
	} .elsewhen(sampling && s_recv) {
		buffer := Cat(buffer(8, 0), data)
		recv_cnt := recv_cnt + 1.U
	} .elsewhen(sampling && state === s_stop) {
		recv_succ := true.B
	}
	when(recv_succ) {
		when(fifohead + 1.U =/= fifotail) {
			fifo(fifohead) := buffer(7, 0)
			fifohead := fifohead + 1.U
		}
		recv_succ := false.B
	}
	when(io.in.psel) {
		when(io.in.penable) {
			when(!io.in.pwrite) {
				when(fifohead =/= fifotail) {
					io.in.prdata := Cat(0.U(24.W), fifo(fifotail))
					fifotail := fifotail + 1.U
					io.in.pready := true.B
				}
			}
		}
	}
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
