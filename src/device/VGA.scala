package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class FrameBuffer extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val clk = Input(Clock())
		val we = Input(Bool())
		val addr_r = Input(UInt(19.W))
		val addr_w = Input(UInt(19.W))
		val data_in = Input(UInt(32.W))
		val data_out = Output(UInt(32.W))
	})
	setInline("FrameBuffer.sv",
	"""
	|module FrameBuffer(
	|	input clk,
	|	input we,
	|	input [18:0] addr_r,
	|	input [18:0] addr_w,
	|	input [31:0] data_in,
	|	output [31:0] data_out
	|);
	|	reg [31:0] mem [0:307199];
	|	always @(posedge clk) begin
	|		if(we) mem[addr_w] <= data_in;
	|	end
	|	assign data_out = mem[addr_r];
	|endmodule
	""".stripMargin
	)
}

class vgaChisel extends Module {
	val io = IO(new VGACtrlIO)
	val frame_buffer = Module(new FrameBuffer)
	
	val apb = io.in
	val vga = io.vga
	apb.pready := false.B
	apb.prdata := 0.U
	apb.pslverr := false.B

	vga.hsync := false.B
	vga.vsync := false.B

	val pixel_ptr = RegInit(0.U(19.W))

	frame_buffer.io.clk := io.clock
	frame_buffer.io.we := false.B
	frame_buffer.io.addr_r := pixel_ptr
	frame_buffer.io.addr_w := 0.U
	frame_buffer.io.data_in := 0.U

	when(apb.psel && apb.penable && apb.pwrite) {
		frame_buffer.io.we := true.B
		frame_buffer.io.addr_w := apb.paddr(20, 2)
		frame_buffer.io.data_in := apb.pwdata
		apb.pready := true.B
	}
	// transmit one pixel per cycle
	pixel_ptr := Mux((pixel_ptr + 1.U >= (640 * 480).U), 0.U, pixel_ptr + 1.U)
	vga.r := frame_buffer.io.data_out(23, 16)
	vga.g := frame_buffer.io.data_out(15, 8)
	vga.b := frame_buffer.io.data_out(7, 0)
	vga.valid := true.B
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
