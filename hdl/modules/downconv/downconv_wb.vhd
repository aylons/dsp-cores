-------------------------------------------------------------------------------
-- Title      : Downconverter (wishbone stream version)
-- Project    : 
-------------------------------------------------------------------------------
-- File       : downconv_wb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2015-01-14
-- Last update: 2015-02-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: This is a downconverter for 4 signals, converting each signal
-- from the original modulated form to the a serialized ABCD signal.
-------------------------------------------------------------------------------
-- Copyright (c) 2015 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2015-01-14  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.wb_stream_pkg.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity downconv_wb is
  generic (
    -- Widths for different busses
    g_input_width  : natural := 16;
    g_mixed_width  : natural := 24;
    g_output_width : natural := 32;
    g_adr_in_width : natural := 2;
    g_tgd_in_width : natural := 2;

    -- Parameters for mixer
    g_sin_file         : string  := "./dds_sin.nif";
    g_cos_file         : string  := "./dds_cos.nif";
    g_dds_width        : natural := 16;
    g_number_of_points : natural := 6;
    g_mixer_levels     : natural := 7;

    -- CIC parameters
    g_cic_diff_delay  : natural := 1;
    g_cic_stages      : natural := 1;
    g_decimation_rate : natural := 1000;

    -- Miscelaneous
    g_std_buffer : natural := 4         -- standard buffer size for each block
    );

  port (
    clk_i : in  std_logic;
    rst_i : in  std_logic;
    ce_i  : in  std_logic;
    snk_i : in  t_wbs_sink_in_array(3 downto 0);
    snk_o : out t_wbs_sink_out_array(3 downto 0);
    src_i : in  t_wbs_source_in_array(3 downto 0);
    src_o : out t_wbs_source_out_array(3 downto 0)
    );

end entity downconv_wb;

architecture str of downconv_wb is

  constant c_cic_bus_width : natural := natural(ceil(log2(real(g_decimation_rate))));

  --Intermediate WBS signals
  signal mixed_snk_i : t_wbs_sink_in_array(3 downto 0);
  signal mixed_snk_o : t_wbs_sink_out_array(3 downto 0);

  signal decimated_snk_i : t_wbs_sink_in_array(3 downto 0);
  signal decimated_snk_o : t_wbs_sink_out_array(3 downto 0);

  signal muxed_snk_i : t_wbs_sink_in;
  signal muxed_snk_o : t_wbs_sink_out;

  component mixer_wb is
    generic (
      g_sin_file         : string;
      g_cos_file         : string;
      g_number_of_points : natural;
      g_input_width      : natural;
      g_sine_width       : natural;
      g_output_width     : natural;
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

  component cic_dual_wb is
    generic (
      g_input_width   : natural;
      g_output_width  : natural;
      g_stages        : natural;
      g_delay         : natural;
      g_decim_rate    : natural;
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
  end component cic_dual_wb;

  component cordic_vectoring_wb is
    generic (
      g_stages        : natural;
      g_width         : natural;
      g_simultaneous  : natural;
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

  component wb_mux is
    generic (
      g_input_number  : natural;
      g_dat_width     : natural;
      g_tgd_width     : natural;
      g_adr_in_width  : natural;
      g_input_buffer  : natural;
      g_output_buffer : natural);
    port (
      clk_i : in  std_logic;
      rst_i : in  std_logic;
      snk_i : in  t_wbs_sink_in_array(g_input_number-1 downto 0);
      snk_o : out t_wbs_sink_out_array(g_input_number-1 downto 0);
      src_i : in  t_wbs_source_in;
      src_o : out t_wbs_source_out);
  end component wb_mux;

begin

  gen_mainchain : for j in 0 to 3 generate

    cmp_mixer : mixer_wb
      generic map (
        g_sin_file         => g_sin_file,
        g_cos_file         => g_cos_file,
        g_number_of_points => g_number_of_points,
        g_input_width      => g_input_width,
        g_sine_width       => g_dds_width,
        g_output_width     => g_mixed_width,
        g_mult_levels      => g_mixer_levels,
        g_tgd_width        => g_tgd_in_width,
        g_adr_width        => g_adr_in_width,
        g_input_buffer     => g_std_buffer,
        g_output_buffer    => g_std_buffer,
        g_simultaneous     => g_mixer_levels+1)
      port map (
        clk_i => clk_i,
        rst_i => rst_i,
        ce_i  => ce_i,
        snk_i => snk_i(j),
        snk_o => snk_o(j),
        src_i => mixed_snk_o(j),
        src_o => mixed_snk_i(j));

    cmp_cic_dual : cic_dual_wb
      generic map (
        g_input_width   => g_mixed_width,
        g_output_width  => g_output_width,
        g_stages        => g_cic_stages,
        g_delay         => g_cic_diff_delay,
        g_decim_rate    => g_decimation_rate,
        g_tgd_width     => g_tgd_in_width,
        g_adr_width     => g_adr_in_width,
        g_input_buffer  => g_std_buffer,
        g_output_buffer => g_std_buffer)
      port map (
        clk_i => clk_i,
        rst_i => rst_i,
        ce_i  => ce_i,
        snk_i => mixed_snk_i(j),
        snk_o => mixed_snk_o(j),
        src_o => src_o(j),
        src_i => src_i(j));
    --src_i => decimated_snk_o(j),
    --src_o => decimated_snk_i(j));

    --cmp_mux : wb_mux
    --  generic map (
    --    g_input_number  => 4,
    --    g_dat_width     => g_output_width*2,
    --    g_tgd_width     => g_tgd_in_width,
    --    g_adr_in_width  => g_adr_in_width,
    --    g_input_buffer  => g_std_buffer+2,
    --    g_output_buffer => g_std_buffer+2)
    --  port map (
    --    clk_i => clk_i,
    --    rst_i => rst_i,
    --    snk_i => decimated_snk_i,
    --    snk_o => decimated_snk_o,
    --    src_i => muxed_snk_o,
    --    src_o => muxed_snk_i);

  --cmp_cordic : cordic_vectoring_wb
  --  generic map (
  --    g_stages        => g_output_width,
  --    g_width         => g_output_width,
  --    g_simultaneous  => 4,
  --    g_tgd_width     => g_tgd_in_width,
  --    g_adr_width     => g_adr_in_width+2,
  --    g_input_buffer  => g_std_buffer,
  --    g_output_buffer => g_std_buffer)
  --  port map (
  --    clk_i => clk_i,
  --    rst_i => rst_i,
  --    ce_i  => ce_i,
  --    snk_i => muxed_snk_i,
  --    snk_o => muxed_snk_o,
  --    src_i => src_i,
  --    src_o => src_o);
  end generate gen_mainchain;

  
end architecture str;
