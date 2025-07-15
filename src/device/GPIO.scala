package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class gpioChisel extends Module {
	val io = IO(new GPIOCtrlIO)
	
	val led = RegInit(0.U(16.W))
	val seg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))

	io.in.pready := false.B
	io.in.pslverr := false.B
	io.in.prdata := 0.U

	when(io.in.psel) {
		when(io.in.penable) {
			when(io.in.pwrite) {
				when(io.in.paddr(3, 0) === 0.U) {
					led := Cat(Mux(io.in.pstrb(1), io.in.pwdata(15, 8), led(15, 8)),
						Mux(io.in.pstrb(0), io.in.pwdata(7, 0), led(7, 0)))
				} .elsewhen(io.in.paddr(3, 0) === 8.U) {
					seg(0) := Mux(io.in.pstrb(0), Cat(0.U(4.W), io.in.pwdata(3, 0)), seg(0))
					seg(1) := Mux(io.in.pstrb(0), Cat(0.U(4.W), io.in.pwdata(7, 4)), seg(1))
					seg(2) := Mux(io.in.pstrb(1), Cat(0.U(4.W), io.in.pwdata(11, 8)), seg(2))
					seg(3) := Mux(io.in.pstrb(1), Cat(0.U(4.W), io.in.pwdata(15, 12)), seg(3))
					seg(4) := Mux(io.in.pstrb(2), Cat(0.U(4.W), io.in.pwdata(19, 16)), seg(4))
					seg(5) := Mux(io.in.pstrb(2), Cat(0.U(4.W), io.in.pwdata(23, 20)), seg(5))
					seg(6) := Mux(io.in.pstrb(3), Cat(0.U(4.W), io.in.pwdata(27, 24)), seg(6))
					seg(7) := Mux(io.in.pstrb(3), Cat(0.U(4.W), io.in.pwdata(31, 28)), seg(7))
				}
				io.in.pready := true.B
				io.in.pslverr := false.B
			} .otherwise {
				when(io.in.paddr(3, 0) === 0.U) {
					io.in.prdata := Cat(0.U(16.W), led)
				} .elsewhen(io.in.paddr(3, 0) === 4.U) {
					io.in.prdata := Cat(0.U(16.W), io.gpio.in)
				} .elsewhen(io.in.paddr(3, 0) === 8.U) {
					io.in.prdata := Cat(seg(7)(3, 0), seg(6)(3, 0), seg(5)(3, 0), seg(4)(3, 0),
						seg(3)(3, 0), seg(2)(3, 0), seg(1)(3, 0), seg(0)(3, 0))
				}
				io.in.pready := true.B
				io.in.pslverr := false.B
			}
		}
	}

	io.gpio.out := led
	io.gpio.seg := seg
}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
