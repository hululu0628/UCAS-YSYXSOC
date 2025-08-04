package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

trait SDRAMCMD {
	def INHIBIT(cmd: UInt) = cmd(3) === 1.U(1.W)
	def NOP(cmd: UInt) = cmd === 7.U(4.W)
	def ACTIVE(cmd: UInt) = cmd === 3.U(4.W)
	def READ(cmd: UInt) = cmd === 5.U(4.W)
	def WRITE(cmd: UInt) = cmd === 4.U(4.W)
	def PRECHARGE(cmd: UInt) = cmd === 2.U(4.W)
	def REFRESH(cmd: UInt) = cmd === 1.U(4.W)
	def MODE(cmd: UInt) = cmd === 0.U(4.W)
}

trait SDRAMMODE {
	def WB(mode: UInt) = mode(9)
	def OPMODE(mode: UInt) = mode(8, 7)
	def CASLat(mode: UInt) = mode(6, 4) // CAS Latency
	def BT(mode: UInt) = mode(3)
	def BL(mode: UInt) = mode(2, 0) // Burst Length
	def toLength(mode: UInt) = {
		// 000: 1, 001: 2, 010: 4, 011: 8, 100: 16
		MuxLookup(BL(mode), 1.U)(Seq(
			1.U -> 2.U,
			2.U -> 4.U,
			3.U -> 8.U
		))
	}
}

class SDRAMIO extends Bundle {
  val sel = Output(Bool())
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(4.W))
  val dq00 = Analog(16.W)
  val dq01 = Analog(16.W)
  val dq10 = Analog(16.W)
  val dq11 = Analog(16.W)
}

class SDRAMIOBlock extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(4.W))
  val dq0  = Analog(16.W)
  val dq1  = Analog(16.W)
}

class SDRAMIOSub extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val dq  = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramChisel extends RawModule {
	val io = IO(Flipped(new SDRAMIO))
	val sdramBlockVec = Seq.fill(2)(Module(new sdramChiselBlock))

	sdramBlockVec(0).io.clk := io.clk
	sdramBlockVec(0).io.cke := io.cke
	sdramBlockVec(0).io.ras := io.ras
	sdramBlockVec(0).io.cas := io.cas
	sdramBlockVec(0).io.we := io.we
	sdramBlockVec(0).io.a := io.a
	sdramBlockVec(0).io.ba := io.ba
	sdramBlockVec(0).io.dqm := io.dqm
	sdramBlockVec(0).io.dq0 <> io.dq00
	sdramBlockVec(0).io.dq1 <> io.dq01

	sdramBlockVec(1).io.clk := io.clk
	sdramBlockVec(1).io.cke := io.cke
	sdramBlockVec(1).io.ras := io.ras
	sdramBlockVec(1).io.cas := io.cas
	sdramBlockVec(1).io.we := io.we
	sdramBlockVec(1).io.a := io.a
	sdramBlockVec(1).io.ba := io.ba
	sdramBlockVec(1).io.dqm := io.dqm
	sdramBlockVec(1).io.dq0 <> io.dq10
	sdramBlockVec(1).io.dq1 <> io.dq11

	sdramBlockVec(0).io.cs := io.cs | io.sel
	sdramBlockVec(1).io.cs := io.cs | !io.sel
}

class sdramChiselBlock extends RawModule {
	val io = IO(Flipped(new SDRAMIOBlock))
	val sdramVec = Seq(Module(new sdramChiselSub(0.U)), Module(new sdramChiselSub(1.U)))

	sdramVec(0).io.clk := io.clk
	sdramVec(0).io.cke := io.cke
	sdramVec(0).io.cs := io.cs
	sdramVec(0).io.ras := io.ras
	sdramVec(0).io.cas := io.cas
	sdramVec(0).io.we := io.we
	sdramVec(0).io.a := io.a
	sdramVec(0).io.ba := io.ba
	sdramVec(0).io.dqm := io.dqm(1, 0)
	sdramVec(0).io.dq <> io.dq0

	sdramVec(1).io.clk := io.clk
	sdramVec(1).io.cke := io.cke
	sdramVec(1).io.cs := io.cs
	sdramVec(1).io.ras := io.ras
	sdramVec(1).io.cas := io.cas
	sdramVec(1).io.we := io.we
	sdramVec(1).io.a := io.a
	sdramVec(1).io.ba := io.ba
	sdramVec(1).io.dqm := io.dqm(3, 2)
	sdramVec(1).io.dq <> io.dq1
}

class sdramChiselSub(idx: UInt) extends RawModule with SDRAMCMD with SDRAMMODE {
	val io = IO(Flipped(new SDRAMIOSub))
	val sdram = Module(new DPI_sdram)

	val dout = Wire(UInt(16.W))
	val out_en = Wire(Bool())
	val di = TriStateInBuf(io.dq, dout, out_en)

	val clk = io.clk.asClock
	withClock(clk) {
		sdram.io.clk := clk
		sdram.io.valid := false.B
		sdram.io.addr := 0.U
		sdram.io.mask := 0.U
		sdram.io.wdata := 0.U
		sdram.io.we := false.B

		out_en := false.B

		dout := 0.U(16.W)

		val s_idle :: s_read :: s_write :: Nil = Enum(3)
		val state = Reg(UInt(2.W))
		val csa_cnt = Reg(UInt(3.W))
		val br_cnt = Reg(UInt(4.W))
		val mask = Reg(UInt(2.W))
		val cmd = Cat(io.cs, io.ras, io.cas, io.we)
		val addrVec = Seq.fill(4)(Reg(UInt(25.W)))
		val mode = Reg(UInt(13.W))

		when(MODE(cmd) && io.cke) {
			mode := io.a
		}

		when(ACTIVE(cmd) && io.cke) {
			for(i <- 0 until 4) {
				when(io.ba === i.U) {
					addrVec(i) := Cat(io.a, io.ba, 0.U(10.W))
				}
			}
			state := s_idle
		}

		when(READ(cmd) && io.cke) {
			for(i <- 0 until 4) {
				when(io.ba === i.U) {
					addrVec(i) := Cat(addrVec(i)(24, 10), io.a(9, 0))
				}
			}
			state := s_read
			csa_cnt := CASLat(mode)
			br_cnt := toLength(mode)
			mask := io.dqm

			sdram.io.valid := true.B
			sdram.io.addr := MuxLookup(io.ba, addrVec(0))(Seq(
				0.U -> Cat(addrVec(0)(24, 10), io.a(9, 1), idx(0)),
				1.U -> Cat(addrVec(1)(24, 10), io.a(9, 1), idx(0)),
				2.U -> Cat(addrVec(2)(24, 10), io.a(9, 1), idx(0)),
				3.U -> Cat(addrVec(3)(24, 10), io.a(9, 1), idx(0))
			))
			sdram.io.mask := io.dqm
			sdram.io.wdata := 0.U
			sdram.io.we := false.B
			dout := sdram.io.rdata(15, 0)
			out_en := true.B
		} .elsewhen(state === s_read && io.cke) {
			sdram.io.valid := false.B
			sdram.io.wdata := 0.U
			sdram.io.we := false.B
			dout := sdram.io.rdata(15, 0)
			out_en := true.B
			state := s_idle
		}

		when(WRITE(cmd) && io.cke) {
			for(i <- 0 until 4) {
				when(io.ba === i.U) {
					addrVec(i) := Cat(addrVec(i)(24, 10), io.a(9, 0))
				}
			}
			state := s_write
			br_cnt := toLength(mode) - 1.U
			mask := io.dqm

			sdram.io.valid := true.B
			sdram.io.addr := MuxLookup(io.ba, addrVec(0))(Seq(
				0.U -> Cat(addrVec(0)(24, 10), io.a(9, 1), idx(0)),
				1.U -> Cat(addrVec(1)(24, 10), io.a(9, 1), idx(0)),
				2.U -> Cat(addrVec(2)(24, 10), io.a(9, 1), idx(0)),
				3.U -> Cat(addrVec(3)(24, 10), io.a(9, 1), idx(0))
			))
			sdram.io.mask := io.dqm
			sdram.io.wdata := di
			sdram.io.we := true.B
		} .elsewhen(state === s_write && io.cke) {
			sdram.io.valid := false.B
			sdram.io.wdata := di
			sdram.io.we := true.B
			out_en := false.B
			state := s_idle
		}
	}
}

class DPI_sdram extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val clk = Input(Clock())
		val valid = Input(Bool())
		val addr = Input(UInt(25.W))
		val mask = Input(UInt(2.W))
		val wdata = Input(UInt(16.W))
		val we = Input(Bool())
		val rdata = Output(UInt(32.W))
	})
	setInline("DPI_sdram.sv",
		"""
		|module DPI_sdram(
		|	input clk,
		|	input valid,
		|	input [24:0] addr,
		|	input [1:0] mask,
		|	input [15:0] wdata,
		|	input we,
		|	output reg [31:0] rdata
		|);
		|import "DPI-C" function void sdram_read(input int addr, output int rdata);
		|import "DPI-C" function void sdram_write(input int addr, input int wdata, input int mask);
		|always @(posedge clk) begin
		|	rdata = 32'h0;
		|	if (valid) begin
		|		if (we) sdram_write({6'b101000, addr, 1'b0}, {16'h0, wdata}, {30'h0, mask});
		|		else sdram_read({6'b101000, addr, 1'b0}, rdata);
		|	end
		|end
		|endmodule
	  """.stripMargin)
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 4
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
        address       = address,
        executable    = true,
        supportsWrite = TransferSizes(1, beatBytes),
        supportsRead  = TransferSizes(1, beatBytes),
        interleavedId = Some(0))
    ),
    beatBytes  = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
