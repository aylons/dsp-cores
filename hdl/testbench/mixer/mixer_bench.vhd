-------------------------------------------------------------------------------
-- Title      : Mixer testbench
-- Project    : 
-------------------------------------------------------------------------------
-- File       : mixer_bench.vhd
-- Author     : Gustavo BM Bruno
-- Company    : LNLS
-- Created    : 2014-01-21
-- Last update: 2014-01-24
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Tests the mixer stage of the BPM DSP chain.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-01-21  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library std;
use std.textio.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity mixer_bench is
end mixer_bench;

architecture test of mixer_bench is

  constant clock_period : time := 3.846 ns; --Half a period of a 130MHz clock
  signal clock     : std_logic := '0';
  signal adc_data  : std_logic_vector(31 downto 0);
  signal endoffile : bit       := '0';

  signal I_sig : std_logic_vector(31 downto 0);
  signal Q_sig : std_logic_vector(31 downto 0);

  component mixer is
    port(
      rst     : in  std_logic;
      clk : in  std_logic;
      input   : in  std_logic_vector(31 downto 0);
      I_out   : out std_logic_vector(31 downto 0);
      Q_out   : out std_logic_vector(31 downto 0));
  end component;
  
begin

  clk_gen : process
  begin
    clock <= '0';
    wait for clock_period;
    clock <= '1';
    wait for clock_period;
  end process;

  adc_read : process(clock)
    file adc_file     : text open read_mode is "amostras.dat";
    variable cur_line : line;
    variable datain   : real;
  begin
    if rising_edge(clock) then
      if not endfile(adc_file) then
        readline(adc_file, cur_line);
        read(cur_line, datain);
        adc_data <= std_logic_vector(to_signed(integer(datain*(2.0**31.0)), 32));
      else
        endoffile <= '1';
      end if;
    end if;
  end process adc_read;

  uut : mixer
    port map(
      rst   => '1',
      clk   => clock,
      input => adc_data,
      I_out => I_sig,
      Q_out => Q_sig);


  signal_write : process(clock)
    file mixer_file   : text open write_mode is "mixer_out.dat";
    variable cur_line : line;
    variable I, Q     : integer;
  begin
    if rising_edge(clock) then
      if(endoffile = '0') then
        I := to_integer(signed(I_sig));
        write(cur_line, I);

        write(cur_line, string'(" "));

        Q := to_integer(signed(Q_sig));
        write(cur_line, Q);
        writeline(mixer_file, cur_line);
      else
        assert (false) report "Input file finished." severity failure;
      end if;
    end if;
  end process;
  
end test;
