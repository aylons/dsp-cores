-------------------------------------------------------------------------------
-- Title      : Wishbone stream FPGA test
-- Project    : 
-------------------------------------------------------------------------------
-- File       : wb_test.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-09-16
-- Last update: 2014-09-19
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Testbench for analysing the performance of wishbone stream modules.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-09-16  1.0      aylons  Created
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.wb_stream_pkg.all;


entity wb_test is
  port(
    sys_clk_p_i : in std_logic;
    sys_clk_n_i : in std_logic;

    rst_i : in  std_logic;
    ce_i  : in  std_logic;
    snk_i : in  t_wbs_sink_in;
    snk_o : out t_wbs_sink_out;
    src_i : in  t_wbs_source_in;
    src_o : out t_wbs_source_out
    );

end entity wb_test;

architecture structural of wb_test is

  -- component generics
  constant c_stages        : natural := 10;
  constant c_width         : natural := 10;
  constant c_simultaneous  : natural := 4;
  constant c_parallel      : boolean := true;
  constant c_ce_factor     : natural := 2;
  constant c_tgd_width     : natural := 128;
  constant c_adr_width     : natural := 4;
  constant c_input_buffer  : natural := 4;
  constant c_output_buffer : natural := 2;

  -- component ports
  signal clock : std_logic;
  signal rst   : std_logic;
  signal ce    : std_logic;

  signal snk_i_middle : t_wbs_sink_in;
  signal snk_o_middle : t_wbs_sink_out;

  signal sys_clk_gen : std_logic;
  signal locked      : std_logic;


  component clk_gen is
    port (
      sys_clk_p_i : in  std_logic;
      sys_clk_n_i : in  std_logic;
      sys_clk_o   : out std_logic);
  end component clk_gen;

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

begin

  clock <= sys_clk_p_i;

  cordic_vectoring_wb_1 : entity work.cordic_vectoring_wb
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
      rst_i => rst_i,
      ce_i  => ce_i,
      snk_i => snk_i,
      snk_o => snk_o,
      src_i => snk_o_middle,
      src_o => snk_i_middle);

  cordic_vectoring_wb_2 : entity work.cordic_vectoring_wb
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
      rst_i => rst_i,
      ce_i  => ce_i,
      snk_i => snk_i_middle,
      snk_o => snk_o_middle,
      src_i => src_i,
      src_o => src_o);

end architecture structural;

