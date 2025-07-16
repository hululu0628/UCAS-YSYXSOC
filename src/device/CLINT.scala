package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class CLINTIO extends Bundle {
}

class CLINTCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val clint = new CLINTIO
}

class clintChisel extends Module {
	val io = IO(new CLINTCtrlIO)
	io.in.pready := false.B
	io.in.prdata := 0.U
	io.in.pslverr := false.B
	val mtime = RegInit(0.U(64.W))
	mtime := mtime + 1.U
	when(io.in.psel) {
		when(io.in.penable && ~io.in.pwrite) {
			io.in.pready := true.B
			when(io.in.paddr === 0x02000000.U) {
				io.in.prdata := mtime(31, 0)
			} .elsewhen(io.in.paddr === 0x02000004.U) {
				io.in.prdata := mtime(63, 32)
			} .otherwise {
				io.in.prdata := 0.U
			}
		}
	}
}

class APBCLINT(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val clint_bundle = IO(new CLINTIO)

    val mclint = Module(new clintChisel)
    mclint.io.clock := clock
    mclint.io.reset := reset
    mclint.io.in <> in
    clint_bundle <> mclint.io.clint
  }
}
