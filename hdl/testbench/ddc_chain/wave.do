onerror {resume}
quietly WaveActivateNextPane {} 0
-- add wave -noupdate /ddc_bench/adc_data
log //uut/*
log //uut/cmp_conditioner/*
