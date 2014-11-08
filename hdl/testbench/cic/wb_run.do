vcom cic_dual_wb_tb.vhd
-- make -f Makefile
-- output log file to file "output.log", set simulation resolution to "fs"
vsim -l output.log -t 1ps -L unisim work.cic_dual_wb_tb -voptargs="+acc"
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
do wb_wave.do
radix -hexadecimal
run -all
wave zoomfull
radix -hexadecimal
