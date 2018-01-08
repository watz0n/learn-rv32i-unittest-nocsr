//=======================================================================
// RISCV, A Simple As Possible Core
// Watson Huang
// Dec 18, 2017
// 
// Combine a core and a magic ram (mram) into a tile
//=======================================================================
package rvtile

import chisel3._
import chisel3.util._
import rvcommon._
import rvcore._

class tile_io extends Bundle {
    val rfdbg = Flipped(new mram_io(rvspec.xlen)) //RegFile DeBuG
    val mrdbg = Flipped(new mram_io(rvspec.xlen)) //MagicRam DeBuG

    val rst_core = Input(Bool()) //ReSeT CORE
}

class rvtile extends Module {

    val io = IO(new tile_io)

    val core = Module(new rvcore())
    val amem = Module(new mram_async())

    core.io.imem <> amem.io.inst_port
    core.io.dmem <> amem.io.data_port

    io.rfdbg <> core.io.rfdbg
    io.mrdbg <> amem.io.dbg_port

    //Keep core in reset state before push all operations to memory
    //Ref: https://github.com/ucb-bar/riscv-sodor/blob/master/src/rv32_1stage/tile.scala
    core.reset := io.rst_core | reset.toBool

}