-------------------------------------------------------------------------------
-- Title      : Testbench for design "mixer_wb"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : mixer_wb_tb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-09-04
-- Last update: 2014-11-06
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-10-11  1.0      aylons  Created
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

entity mixer_wb_tb is

end entity mixer_wb_tb;

-------------------------------------------------------------------------------

architecture test of mixer_wb_tb is

  constant input_freq   : real    := 120.0e6;
  constant clock_period : time    := 1.0 sec /(2.0*input_freq);
  constant ce_period    : natural := 1;
  constant reset_clocks : natural := 8;

  -- component generics
  constant c_sin_file         : string  := "./ones.nif";
  constant c_cos_file         : string  := "./ones.nif";
  constant c_number_of_points : natural := 10;
  constant c_phase_bus_size   : natural := 1;
  constant c_input_width      : natural := 16;
  constant c_output_width     : natural := 32;
  constant c_sine_width       : natural := 16;
  constant c_mult_levels      : natural := 7;
  constant c_tgd_width        : natural := c_input_width+1;
  constant c_adr_width        : natural := 2;
  constant c_input_buffer     : natural := 4;
  constant c_output_buffer    : natural := 2;
  constant c_simultaneous     : natural := c_mult_levels+2;

  -- component ports
  signal clock : std_logic;
  signal rst   : std_logic := '1';
  signal ce    : std_logic := '1';
  signal snk_i : t_wbs_sink_in;
  signal snk_o : t_wbs_sink_out;
  signal src_i : t_wbs_source_in;
  signal src_o : t_wbs_source_out;

  -- Signals
  signal tgd_input   : std_logic_vector(c_tgd_width-1 downto 0) := (others => '0');
  signal adr_input   : std_logic_vector(c_adr_width-1 downto 0) := (others => '0');
  signal valid_input : std_logic                                := '0';

  signal tgd_output   : std_logic_vector(c_tgd_width-1 downto 0) := (others => '0');
  signal adr_output   : std_logic_vector(c_adr_width-1 downto 0) := (others => '0');
  signal valid_output : std_logic                                := '0';

  component mixer_wb is
    generic (
      g_sin_file         : string;
      g_cos_file         : string;
      g_number_of_points : natural;
      g_phase_bus_size   : natural;
      g_input_width      : natural;
      g_output_width     : natural;
      g_sine_width       : natural;
      g_mult_levels      : natural;
      g_tgd_width        : natural;
      g_adr_width        : natural;
      g_input_buffer     : natural;
      g_output_buffer    : natural;
      g_simultaneous     : natural);
    port (
      clk_i : in  std_logic;
      rst_i : in  std_logic;
      ce_i  : in  std_logic;
      snk_i : in  t_wbs_sink_in;
      snk_o : out t_wbs_sink_out;
      src_i : in  t_wbs_source_in;
      src_o : out t_wbs_source_out);
  end component mixer_wb;
  
begin  -- architecture test

  clk_gen : process
  begin
    clock <= '0';
    wait for clock_period;
    clock <= '1';
    wait for clock_period;
  end process;

  rst_gen : process(clock)
    variable rst_count : natural := reset_clocks;
  begin
    if rising_edge(clock) then
      if rst_count > 0 then
        rst_count := rst_count-1;
      else
        rst <= '0';
      end if;
    end if;
  end process rst_gen;


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

  uut : mixer_wb
    generic map (
      g_sin_file         => c_sin_file,
      g_cos_file         => c_cos_file,
      g_number_of_points => c_number_of_points,
      g_phase_bus_size   => c_phase_bus_size,
      g_input_width      => c_input_width,
      g_output_width     => c_output_width,
      g_sine_width       => c_sine_width,
      g_mult_levels      => c_mult_levels,
      g_tgd_width        => c_tgd_width,
      g_adr_width        => c_adr_width,
      g_input_buffer     => c_input_buffer,
      g_output_buffer    => c_output_buffer,
      g_simultaneous     => c_simultaneous)
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
    variable count : natural := 0;
    --variable

    variable count_slv, count_tgd_slv : std_logic_vector(c_input_width-1 downto 0) := (others => '0');

--    variable input, input_tgd : std_logic_vector(c_width-1 downto 0) := (others => '0');

    variable I_out : std_logic_vector(c_output_width-1 downto 0) := (others => '0');
    variable Q_out : std_logic_vector(c_output_width-1 downto 0) := (others => '0');

    variable I_out_int, Q_out_int, count_tgd_int : integer;

  begin
    src_i.stall <= '0';

    loop
      wait until clock = '1' and rst = '0';
      if snk_o.stall = '0' then

        count     := count + 100;
        count_slv := std_logic_vector(to_signed(count, c_input_width));

        snk_i.dat(c_wbs_data_width-1 downto c_input_width) <= (others => '0');
        snk_i.dat(c_input_width-1 downto 0)                <= count_slv;

        snk_i.adr <= (others => '1');

        snk_i.tgd <= (c_wbs_tgd_width-1 downto c_tgd_width-1 => '0') & count_slv;

        snk_i.stb <= '1';
        snk_i.cyc <= '1';
      else
        snk_i.stb <= '0';
        snk_i.cyc <= '0';

      end if;

      -- output 
      if (src_o.cyc and src_o.stb) = '1' then

        I_out := src_o.dat(c_output_width*2-1 downto c_output_width);
        Q_out := src_o.dat(c_output_width-1 downto 0);

        count_tgd_slv := src_o.tgd(c_input_width downto 1);
        count_tgd_int := to_integer(signed(count_tgd_slv));

        Q_out_int := to_integer(signed(Q_out));
        I_out_int := to_integer(signed(I_out));

      end if;
    end loop;

  end process WaveGen;

end architecture test;
