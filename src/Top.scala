package ysyx

import chisel3._
import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy.LazyModule

object Config {
  def hasChipLink: Boolean = true
  def sdramUseAXI: Boolean = false
}

class Top extends Module {
  implicit val config: Parameters = new Config(new Edge32BitConfig ++ new DefaultRV32Config)

  val io = IO(new Bundle {
	// gpio
	val led = Output(UInt(16.W))
	val sw = Input(UInt(16.W))
	val seg0 = Output(UInt(8.W))
	val seg1 = Output(UInt(8.W))
	val seg2 = Output(UInt(8.W))
	val seg3 = Output(UInt(8.W))
	val seg4 = Output(UInt(8.W))
	val seg5 = Output(UInt(8.W))
	val seg6 = Output(UInt(8.W))
	val seg7 = Output(UInt(8.W))
	// uart
	val uart_tx = Output(UInt(1.W))
	val uart_rx = Input(UInt(1.W))
	// ps2
	val ps2_clk = Input(Bool())
	val ps2_data = Input(Bool())
	// vga
	val vga_valid = Output(Bool())
	val vga_hsync = Output(Bool())
	val vga_vsync = Output(Bool())
	val vga_r = Output(UInt(8.W))
	val vga_g = Output(UInt(8.W))
	val vga_b = Output(UInt(8.W))
  })
	val dut = LazyModule(new ysyxSoCFull)
	val mdut = Module(dut.module)
	mdut.dontTouchPorts()
	mdut.externalPins := DontCare
	// gpio
	io.led := mdut.externalPins.gpio.out
	io.seg0 := mdut.externalPins.gpio.seg(0)
	io.seg1 := mdut.externalPins.gpio.seg(1)
	io.seg2 := mdut.externalPins.gpio.seg(2)
	io.seg3 := mdut.externalPins.gpio.seg(3)
	io.seg4 := mdut.externalPins.gpio.seg(4)
	io.seg5 := mdut.externalPins.gpio.seg(5)
	io.seg6 := mdut.externalPins.gpio.seg(6)
	io.seg7 := mdut.externalPins.gpio.seg(7)
	mdut.externalPins.gpio.in := io.sw
	// uart
	io.uart_tx := mdut.externalPins.uart.tx
	mdut.externalPins.uart.rx := io.uart_rx
	// ps2
	mdut.externalPins.ps2.clk := io.ps2_clk
	mdut.externalPins.ps2.data := io.ps2_data
	// vga
	io.vga_valid := mdut.externalPins.vga.valid
	io.vga_hsync := mdut.externalPins.vga.hsync
	io.vga_vsync := mdut.externalPins.vga.vsync
	io.vga_r := mdut.externalPins.vga.r
	io.vga_g := mdut.externalPins.vga.g
	io.vga_b := mdut.externalPins.vga.b
}

object Elaborate extends App {
  val firtoolOptions = Array("--disable-annotation-unknown")
  circt.stage.ChiselStage.emitSystemVerilogFile(new Top, args, firtoolOptions)
}
