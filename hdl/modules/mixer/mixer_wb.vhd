-------------------------------------------------------------------------------
-- Title      : Wishbonized mixer
-- Project    : 
-------------------------------------------------------------------------------
-- File       : mixer_wb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-10-11
-- Last update: 2014-11-06
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Mixer with wishbone stream interface
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

use work.genram_pkg.all;
use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity mixer_wb is

  generic (
    g_sin_file         : string  := "./dds_sin.nif";
    g_cos_file         : string  := "./dds_cos.nif";
    g_number_of_points : natural := 6;
    g_phase_bus_size   : natural := 8;
    g_input_width      : natural := 24;
    g_sine_width       : natural := 24;
    g_output_width     : natural := 24;
    g_mult_levels      : natural := 7;  -- pipeline level in multiplier

    g_tgd_width     : natural := 4;
    g_adr_width     : natural := 4;
    g_input_buffer  : natural := 4;
    g_output_buffer : natural := 2;
    g_simultaneous  : natural := 4
    );

  port (
    clk_i : in  std_logic;
    rst_i : in  std_logic;
    ce_i  : in  std_logic;
    snk_i : in  t_wbs_sink_in;
    snk_o : out t_wbs_sink_out;
    src_i : in  t_wbs_source_in;
    src_o : out t_wbs_source_out
    );

end entity mixer_wb;

architecture str of mixer_wb is

  signal input_data                : std_logic_vector(g_input_width-1 downto 0);
  signal I_out, Q_out              : std_logic_vector(g_output_width-1 downto 0);
  signal output_data               : std_logic_vector(g_output_width*2-1 downto 0);
  signal ce                        : std_logic;
  signal valid_input, valid_output : std_logic;

  component mixer is
    generic (
      g_sin_file         : string;
      g_cos_file         : string;
      g_number_of_points : natural;
      g_phase_bus_size   : natural;
      g_input_width      : natural;
      g_output_width     : natural;
      g_sine_width       : natural;
      g_mult_levels      : natural);
    port (
      reset_i     : in  std_logic;
      clock_i     : in  std_logic;
      ce_i        : in  std_logic;
      signal_i    : in  std_logic_vector(g_input_width-1 downto 0);
      phase_sel_i : in  std_logic_vector(g_phase_bus_size-1 downto 0);
      I_out       : out std_logic_vector(g_output_width-1 downto 0);
      Q_out       : out std_logic_vector(g_output_width-1 downto 0));
  end component mixer;

  component xwb_simple_wrapper is
    generic (
      g_input_width   : natural;
      g_output_width  : natural;
      g_tgd_width     : natural;
      g_adr_width     : natural;
      g_input_buffer  : natural;
      g_output_buffer : natural;
      g_simultaneous  : natural);
    port (
      clk_i   : in  std_logic;
      rst_i   : in  std_logic;
      ce_i    : in  std_logic;
      snk_i   : in  t_wbs_sink_in;
      snk_o   : out t_wbs_sink_out;
      src_i   : in  t_wbs_source_in;
      src_o   : out t_wbs_source_out;
      ce_o    : out std_logic;
      data_o  : out std_logic_vector(g_input_width-1 downto 0);
      data_i  : in  std_logic_vector(g_output_width-1 downto 0);
      valid_o : out std_logic;
      valid_i : in  std_logic;
      error_o : out std_logic;
      error_i : in  std_logic);
  end component xwb_simple_wrapper;

  component pipeline is
    generic (
      g_width : natural;
      g_depth : natural);
    port (
      data_i : in  std_logic_vector(g_width-1 downto 0);
      clk_i  : in  std_logic;
      ce_i   : in  std_logic;
      data_o : out std_logic_vector(g_width-1 downto 0));
  end component pipeline;
  
begin  -- architecture str

  output_data <= I_out & Q_out;

  cmp_wb_wrapper : xwb_simple_wrapper
    generic map (
      g_input_width   => g_input_width,
      g_output_width  => g_output_width*2,
      g_tgd_width     => g_tgd_width,
      g_adr_width     => g_adr_width,
      g_input_buffer  => g_input_buffer,
      g_output_buffer => g_output_buffer,
      g_simultaneous  => g_simultaneous)
    port map (
      clk_i   => clk_i,
      rst_i   => rst_i,
      ce_i    => ce_i,
      snk_i   => snk_i,
      snk_o   => snk_o,
      src_i   => src_i,
      src_o   => src_o,
      ce_o    => ce,
      data_o  => input_data,
      data_i  => output_data,
      valid_o => valid_input,
      valid_i => valid_output,
      error_o => open,
      error_i => '0');

  cmp_mixer : mixer
    generic map (
      g_sin_file         => g_sin_file,
      g_cos_file         => g_cos_file,
      g_number_of_points => g_number_of_points,
      g_phase_bus_size   => g_phase_bus_size,
      g_input_width      => g_input_width,
      g_output_width     => g_output_width,
      g_sine_width       => g_sine_width,
      g_mult_levels      => g_mult_levels)
    port map (
      reset_i     => rst_i,
      clock_i     => clk_i,
      ce_i        => ce,
      signal_i    => input_data,
      phase_sel_i => (others => '0'),
      I_out       => I_out,
      Q_out       => Q_out);

  cmp_valid_pipe : pipeline
    generic map (
      g_width => 1,
      g_depth => g_mult_levels+2)
    port map (
      data_i(0) => valid_input,
      clk_i     => clk_i,
      ce_i      => ce,
      data_o(0) => valid_output);

end architecture str;

-------------------------------------------------------------------------------
