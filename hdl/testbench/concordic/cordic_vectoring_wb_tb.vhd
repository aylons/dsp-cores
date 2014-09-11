-------------------------------------------------------------------------------
-- Title      : Testbench for design "cordic_vectoring_wb"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : cordic_vectoring_wb_tb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-09-04
-- Last update: 2014-09-11
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-09-04  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library std;
use std.textio.all;

library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity cordic_vectoring_wb_tb is

end entity cordic_vectoring_wb_tb;

-------------------------------------------------------------------------------

architecture test of cordic_vectoring_wb_tb is

  constant input_freq   : real    := 120.0e6;
  constant clock_period : time    := 1.0 sec /(2.0*input_freq);
  constant ce_period    : natural := 4;

  -- component generics
  constant c_stages        : natural := 32;
  constant c_width         : natural := 32;
  constant c_simultaneous  : natural := 4;
  constant c_parallel      : boolean := true;
  constant c_ce_factor     : natural := 2;
  constant c_tgd_width     : natural := 65;
  constant c_adr_width     : natural := 3;
  constant c_input_buffer  : natural := 4;
  constant c_output_buffer : natural := 2;

  -- component ports
  signal clock : std_logic;
  signal rst   : std_logic;
  signal ce    : std_logic;
  signal snk_i : t_wbs_sink_in;
  signal snk_o : t_wbs_sink_out;
  signal src_i : t_wbs_source_in;
  signal src_o : t_wbs_source_out;

  -- Signals
  signal mag   : std_logic_vector(c_width-1 downto 0) := (others => '0');
  signal phase : std_logic_vector(c_width-1 downto 0) := (others => '0');

  signal tgd_input   : std_logic_vector(c_tgd_width-1 downto 0) := (others => '0');
  signal adr_input   : std_logic_vector(c_adr_width-1 downto 0) := (others => '0');
  signal valid_input : std_logic                                := '0';

  signal tgd_output   : std_logic_vector(c_tgd_width-1 downto 0) := (others => '0');
  signal adr_output   : std_logic_vector(c_adr_width-1 downto 0) := (others => '0');
  signal valid_output : std_logic                                := '0';

  component cordic_vectoring_wb is
    generic (
      g_stages        : natural;
      g_width         : natural;
      g_simultaneous  : natural;
      g_parallel      : boolean;
      g_tgd_width     : natural;
      g_adr_width     : natural;
      g_input_buffer  : natural;
      g_output_buffer : natural);
    port (
      clk_i : in  std_logic;
      rst_i : in  std_logic;
      ce_i  : in  std_logic;
      snk_i : in  t_wbs_sink_in;
      snk_o : out t_wbs_sink_out;
      src_i : in  t_wbs_source_in;
      src_o : out t_wbs_source_out);
  end component cordic_vectoring_wb;
  
begin  -- architecture test

  clk_gen : process
  begin
    clock <= '0';
    wait for clock_period;
    clock <= '1';
    wait for clock_period;
  end process;

  ce_gen : process(clock)
    variable ce_count : natural := ce_period;
  begin
    if rising_edge(clock) then
      ce_count := ce_count-1;
      if ce_count = 0 then
        ce       <= '1';
        ce_count := ce_period;
      else
        ce <= '0';
      end if;
    end if;
  end process;

  -- component instantiation
  uut : cordic_vectoring_wb
    generic map (
      g_stages        => c_stages,
      g_width         => c_width,
      g_simultaneous  => c_simultaneous,
      g_parallel      => c_parallel,
      g_tgd_width     => c_tgd_width,
      g_adr_width     => c_adr_width,
      g_input_buffer  => c_input_buffer,
      g_output_buffer => c_output_buffer)
    port map (
      clk_i => clock,
      rst_i => rst,
      ce_i  => ce,
      snk_i => snk_i,
      snk_o => snk_o,
      src_i => src_i,
      src_o => src_o);

  -- waveform generation
  WaveGen : process
    variable count : natural                              := 0;
    --variable
    variable I     : std_logic_vector(c_width-1 downto 0) := (others => '0');
    variable Q     : std_logic_vector(c_width-1 downto 0) := (others => '0');

  begin

    loop
      wait until clock = '1';
      if snk_o.stall = '0' and ce = '1' then
        I                                      := std_logic_vector(to_signed(707, c_width));
        Q                                      := std_logic_vector(to_signed(707, c_width));
        snk_i.dat(c_wbs_tgd_width-1 downto 64) <= (others                    => '0');
        snk_i.dat(63 downto 32)                <= I;
        snk_i.dat(31 downto 0)                 <= Q;
        snk_i.adr                              <= (others                    => '1');
        snk_i.tgd                              <= (c_wbs_tgd_width downto 66 => '0') & I & Q & '0';

        snk_i.stb <= '1';
        snk_i.cyc <= '1';
      else
        snk_i.stb <= '0';
        snk_i.cyc <= '0';
      end if;

      -- output 
      if (src_o.cyc and src_o.stb) = '1' then
        mag   <= src_o.dat(63 downto 32);
        phase <= src_o.dat(31 downto 0);
      end if;
    end loop;

  end process WaveGen;

end architecture test;
