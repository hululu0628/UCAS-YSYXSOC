package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

trait SPIParameters {
	def SPI_RX0 = 0x10001000.U
	def SPI_RX1 = 0x10001004.U
	def SPI_RX2 = 0x10001008.U
	def SPI_RX3 = 0x1000100c.U
	def SPI_TX0 = 0x10001000.U
	def SPI_TX1 = 0x10001004.U
	def SPI_TX2 = 0x10001008.U
	def SPI_TX3 = 0x1000100c.U
	def SPI_CTRL = 0x10001010.U
	def SPI_DIV = 0x10001014.U
	def SPI_SS = 0x10001018.U

	def CTRL_ASS = 0x2000.U
	def CTRL_IE = 0x1000.U
	def CTRL_LSB = 0x800.U
	def CTRL_TXNEG = 0x400.U
	def CTRL_RXNEG = 0x200.U
	def CTRL_GO = 0x100.U

}

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule with SPIParameters{
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
	val spi_bundle = IO(new SPIIO)
	val mspi = Module(new spi_top_apb)

	val apb_io = Wire(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))

	val addrInFlash = in.psel && (in.paddr(29,28) === 0x3.U)

	val s_spi::s_fsetctrl::s_fsetdiv::s_fsettx0::s_fsettx1::s_fsetss::s_fsetgo::s_fcheckgo::s_fgetres::s_fclss::Nil = Enum(10)
	val state = RegInit(s_spi)
	val apb_idle::apb_enable::Nil = Enum(2)
	val apb_state = RegInit(apb_idle)

	state := MuxLookup(state, s_spi)(Seq(
		s_spi -> Mux(addrInFlash, s_fsetctrl, s_spi),
		s_fsetctrl -> Mux(apb_io.penable && mspi.io.in.pready, s_fsetdiv, s_fsetctrl),
		s_fsetdiv -> Mux(apb_io.penable && mspi.io.in.pready, s_fsettx0, s_fsetdiv),
		s_fsettx0 -> Mux(apb_io.penable && mspi.io.in.pready, s_fsettx1, s_fsettx0),
		s_fsettx1 -> Mux(apb_io.penable && mspi.io.in.pready, s_fsetss, s_fsettx1),
		s_fsetss -> Mux(apb_io.penable && mspi.io.in.pready, s_fsetgo, s_fsetss),
		s_fsetgo -> Mux(apb_io.penable && mspi.io.in.pready, s_fcheckgo, s_fsetgo),
		s_fcheckgo -> Mux(apb_io.penable && mspi.io.in.pready && !(mspi.io.in.prdata & CTRL_GO), s_fgetres, s_fcheckgo),
		s_fgetres -> Mux(apb_io.penable && mspi.io.in.pready, s_fclss, s_fgetres),
		s_fclss -> Mux(apb_io.penable && mspi.io.in.pready, 
			    Mux(addrInFlash, s_fsetctrl, s_spi), s_fclss)
	))

	apb_state := MuxLookup(apb_state, apb_idle)(Seq(
		apb_idle -> Mux(apb_io.psel, apb_enable, apb_idle),
		apb_enable -> Mux(apb_io.penable && mspi.io.in.pready, apb_idle, apb_enable)
	))

	when(state === s_spi) {
		apb_io <> in
	} .elsewhen(state === s_fsetctrl) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_CTRL
		apb_io.pwrite := true.B
		apb_io.pwdata := 64.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fsetdiv) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_DIV
		apb_io.pwrite := true.B
		apb_io.pwdata := 10.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fsettx0) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_TX0
		apb_io.pwrite := true.B
		apb_io.pwdata := 0.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fsettx1) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_TX1
		apb_io.pwrite := true.B
		apb_io.pwdata := Cat(0x3.U(8.W), in.paddr(23,0))
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fsetss) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_SS
		apb_io.pwrite := true.B
		apb_io.pwdata := 1.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fsetgo) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_CTRL
		apb_io.pwrite := true.B
		apb_io.pwdata := (CTRL_GO | 64.U)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fcheckgo) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_CTRL
		apb_io.pwrite := false.B
		apb_io.pwdata := 0.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .elsewhen(state === s_fgetres) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_RX0
		apb_io.pwrite := true.B
		apb_io.pwdata := 0.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		in.pready := false.B
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
			when(mspi.io.in.pready) {
				in.pready := true.B
				in.prdata := Cat(apb_io.prdata(7,0), apb_io.prdata(15,8), apb_io.prdata(23,16), apb_io.prdata(31,24))
			}
		}
	} .elsewhen(state === s_fclss) {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_SS
		apb_io.pwrite := true.B
		apb_io.pwdata := 0.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		when(apb_state === apb_enable) {
			apb_io.penable := true.B
		}
		in.pready := false.B
	} .otherwise {
		apb_io.psel := true.B
		apb_io.penable := false.B
		apb_io.paddr := SPI_CTRL
		apb_io.pwrite := true.B
		apb_io.pwdata := 0.U(32.W)
		apb_io.pstrb := 0xf.U(4.W)
		apb_io.pprot := 0.U
		in.pready := false.B
	}

	mspi.io.clock := clock
	mspi.io.reset := reset
	mspi.io.in <> apb_io
	spi_bundle <> mspi.io.spi
  }
}
