package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class APBDelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val out = new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
}

class apb_delayer extends BlackBox {
  val io = IO(new APBDelayerIO)
}

class APBDelayerChisel(rate: Double = 4.75, scale: Int = 4) extends Module {
	val io = IO(new APBDelayerIO)
	val delaycnt = RegInit(0.U(32.W))
	val s_idle :: s_cnt :: s_wait :: Nil = Enum(3)
	val state = RegInit(s_idle)
	val data_buf = RegInit(0.U(32.W))
	state := MuxLookup(state, s_idle)(Seq(
		s_idle -> Mux(io.in.psel, s_cnt, s_idle),
		s_cnt  -> Mux(io.out.pready, Mux(delaycnt === 0.U, s_idle, s_wait), s_cnt),
		s_wait -> Mux(delaycnt === 0.U, s_idle, s_wait)
	))
	when(state === s_wait && delaycnt =/= 0.U) {
		delaycnt := delaycnt - 1.U
	} .elsewhen(io.in.psel && !io.out.pready) {
		delaycnt := delaycnt + ((rate - 1) * scale).asInstanceOf[Int].U
	} .elsewhen(io.out.pready) {
		delaycnt := delaycnt >> log2Ceil(scale).U
	}

	when(io.out.pready) {
		data_buf := io.out.prdata
	}

	io.out <> io.in
	io.in.pready := false.B
	when(state === s_wait) {
		io.out.psel := false.B
		io.out.penable := false.B
		io.in.pready := false.B
		io.in.prdata := data_buf
		when(delaycnt === 0.U) {
			io.in.pready := true.B
		}
	} .elsewhen(state === s_cnt) {
		when(delaycnt === 0.U && io.out.pready) {
			io.in.pready := true.B
		}
	}
}

class APBDelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = APBIdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new APBDelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object APBDelayer {
  def apply()(implicit p: Parameters): APBNode = {
    val apbdelay = LazyModule(new APBDelayerWrapper)
    apbdelay.node
  }
}
