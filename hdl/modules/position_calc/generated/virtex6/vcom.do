-- If you see error messages concerning missing libraries for
-- XilinxCoreLib, unisims, or simprims, you may not have set
-- up your ModelSim environment correctly.  See the Xilinx
-- Support Website for instructions telling how to compile
-- these libraries.

vlib work

vcom  -nowarn 1 ddc_bpm_476_066.vhd
vcom  -nowarn 1 fifo_generator_virtex6_8_4_784d0e5148f6dbe1.vhd
vcom  -nowarn 1 ddc_bpm_476_066_cw.vhd
