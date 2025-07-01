package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel extends RawModule {
	val io = IO(Flipped(new QSPIIO))
	val dout = Wire(UInt(4.W))
	val out_en = Wire(Bool())
	val di = TriStateInBuf(io.dio, dout, out_en) // change this if you need

	val dpi_psram = Module(new DPI_psram)

	val clk = io.sck.asClock
	val reset = io.ce_n.asBool

	val s_cmd::s_addr::r_wait::r_data::w_data::Nil = Enum(5)
	withClockAndReset(clk, reset.asAsyncReset) {
		val state = RegInit(s_cmd)
		val cmd = RegInit(0.U(8.W))
		val addr = RegInit(0.U(24.W))
		val cnt = RegInit(0.U(5.W))
		val data = RegInit(0.U(32.W))

		dpi_psram.io.clk := clk
		dpi_psram.io.valid := false.B
		dpi_psram.io.addr := addr
		dpi_psram.io.wdata := data
		dpi_psram.io.we := false.B

		dout := 0.U(4.W)
		out_en := false.B

		when(state === s_cmd) {
			cmd := (cmd << 1) | Cat(0.U(7.W), di(0))
			when(cnt < 7.U) {
				cnt := cnt + 1.U
			} .otherwise {
				cnt := 0.U
				state := s_addr
			}
		} .elsewhen(state === s_addr) {
			addr := (addr << 4) | Cat(0.U(16.W), di)
			when(cnt < 5.U) {
				cnt := cnt + 1.U
			} .otherwise {
				cnt := 0.U
				when(cmd === 0xEB.U) {
					state := r_wait
				} .elsewhen(cmd === 0x38.U) {
					state := w_data
				}
			}
		} .elsewhen(state === r_wait) {
			when(cnt === 0.U) {
				dpi_psram.io.valid := true.B
			}
			data := dpi_psram.io.rdata
			when(cnt < 6.U) {
				cnt := cnt + 1.U
			} .otherwise {
				cnt := 0.U
				state := r_data
			}
		} .elsewhen(state === r_data) {
			out_en := true.B
			when(cnt < 7.U) {
				cnt := cnt + 1.U
			} .otherwise {
				cnt := 0.U
				state := s_cmd
			}
			when(cnt(0) === 0.U) {
				dout := data(7, 4)
			} .otherwise {
				dout := data(3, 0)
				data := Cat(0.U(8.W), data(31, 8))
			}
		} .elsewhen(state === w_data) {
			cnt := cnt + 1.U
			when(cnt(0) === 0.U) {
				data := Cat(0.U(24.W), di, 0.U(4.W))
			} .otherwise {
				addr := addr + 1.U
				dpi_psram.io.valid := true.B
				dpi_psram.io.we := true.B
				dpi_psram.io.wdata := Cat(data(31, 4), di)
			}
		}
	}
}

class DPI_psram extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val clk = Input(Clock())
		val valid = Input(Bool())
		val addr = Input(UInt(24.W))
		val wdata = Input(UInt(32.W))
		val rdata = Output(UInt(32.W))
		val we = Input(Bool())
	})
	setInline("DPI_psram.sv",
	"""
	|module DPI_psram(
	|	input clk,
	|	input valid,
	|	input [23:0] addr,
	|	input [31:0] wdata,
	|	output reg [31:0] rdata,
	|	input we
	|);
	|import "DPI-C" function void psram_read(input int addr, output int rdata);
	|import "DPI-C" function void psram_write(input int addr, input int wdata);
	|always @(posedge clk) begin
	|	if (valid) begin
	|		if (we) begin
	|			psram_write({8'h80, addr}, wdata);
	|		end else begin
	|			psram_read({8'h80, addr}, rdata);
	|		end
	|	end
	|end
	|endmodule
	""".stripMargin)
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
