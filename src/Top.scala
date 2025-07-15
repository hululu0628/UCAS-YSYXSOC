package ysyx

import chisel3._
import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy.LazyModule

object Config {
  def hasChipLink: Boolean = false
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
}

object Elaborate extends App {
  val firtoolOptions = Array("--disable-annotation-unknown")
  circt.stage.ChiselStage.emitSystemVerilogFile(new Top, args, firtoolOptions)
}
