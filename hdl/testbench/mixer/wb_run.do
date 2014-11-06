vcom mixer_wb_tb.vhd
-- make -f Makefile
-- output log file to file "output.log", set siulation resolution to "fs"
vsim -l output.log -t 1ps -L unisim work.mixer_wb_tb -voptargs="+acc"
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
do wb_wave.do
-- do wave_compl.do
radix -hexadecimal
-- run 250us
-- run 100us
run -all
wave zoomfull
radix -hexadecimal
