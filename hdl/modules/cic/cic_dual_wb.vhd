-------------------------------------------------------------------------------
-- Title      : Dual CIC with wishbone stream interface
-- Project    : 
-------------------------------------------------------------------------------
-- File       : cic_dual_wb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-11-07
-- Last update: 2014-11-08
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Dual cic with wishbone stream interface
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-11-07  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.genram_pkg.all;
use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity cic_dual_wb is
  generic (
    g_input_width  : natural := 16;
    g_output_width : natural := 16;
    g_stages       : natural := 1;      -- aka "N"
    g_delay        : natural := 1;      -- aka "M"
    g_decim_rate   : natural := 1024;   --keep a fixed rate for now

    g_tgd_width     : natural := 4;
    g_adr_width     : natural := 4;
    g_input_buffer  : natural := 4;
    g_output_buffer : natural := 2
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

end entity cic_dual_wb;

-------------------------------------------------------------------------------

architecture str of cic_dual_wb is

  constant c_bit_growth     : natural                                  := integer(ceil(real(g_delay)*log2(real(g_stages)*real(g_decim_rate))));
  constant c_bus_width      : natural                                  := natural(ceil(log2(real(g_decim_rate+1))));
  constant c_ratio_slv      : std_logic_vector(c_bus_width-1 downto 0) := std_logic_vector(to_unsigned(g_decim_rate, c_bus_width));

  constant c_in_data_width  : natural                                  := g_input_width*2;
  constant c_out_data_width : natural                                  := g_output_width*2;

  signal data_in  : std_logic_vector(c_in_data_width-1 downto 0);
  signal data_out : std_logic_vector(c_out_data_width-1 downto 0);

  signal I_in, Q_in   : std_logic_vector(g_input_width-1 downto 0);
  signal I_out, Q_out : std_logic_vector(g_output_width-1 downto 0);

  signal metadata_in, metadata_middle, metadata_out : std_logic_vector(g_adr_width + g_tgd_width-1 downto 0);

  signal ce, ce_valid, ce_strobe   : std_logic;
  signal valid_input, valid_output : std_logic;
  signal source_req                : std_logic;
  signal decimation_strobe         : std_logic;
  signal rst_n                     : std_logic;


  component cic_decim is
    generic (
      DATAIN_WIDTH  : integer;
      DATAOUT_WIDTH : integer;
      M             : integer;
      N             : integer;
      MAXRATE       : integer;
      bitgrowth     : integer);
    port (
      clk_i     : in  std_logic;
      rst_i     : in  std_logic;
      en_i      : in  std_logic;
      data_i    : in  std_logic_vector(DATAIN_WIDTH-1 downto 0);
      data_o    : out std_logic_vector(DATAOUT_WIDTH-1 downto 0);
      act_i     : in  std_logic;
      act_out_i : in  std_logic;
      val_o     : out std_logic);
  end component cic_decim;

  component strobe_gen is
    generic (
      g_maxrate   : natural;
      g_bus_width : natural);
    port (
      clock_i  : in  std_logic;
      reset_i  : in  std_logic;
      ce_i     : in  std_logic;
      ratio_i  : in  std_logic_vector(g_bus_width-1 downto 0);
      strobe_o : out std_logic);
  end component strobe_gen;

  component xwb_stream_sink is
    generic (
      g_data_width   : natural;
      g_addr_width   : natural;
      g_tgd_width    : natural;
      g_buffer_depth : natural);
    port (
      clk_i    : in  std_logic;
      rst_n_i  : in  std_logic;
      snk_i    : in  t_wbs_sink_in;
      snk_o    : out t_wbs_sink_out;
      addr_o   : out std_logic_vector(g_addr_width-1 downto 0);
      data_o   : out std_logic_vector(g_data_width-1 downto 0);
      tgd_o    : out std_logic_vector(g_tgd_width-1 downto 0);
      error_o  : out std_logic;
      dvalid_o : out std_logic;
      dreq_i   : in  std_logic);
  end component xwb_stream_sink;

  component xwb_stream_source is
    generic (
      g_data_width   : natural;
      g_addr_width   : natural;
      g_tgd_width    : natural;
      g_buffer_depth : natural);
    port (
      clk_i    : in  std_logic;
      rst_n_i  : in  std_logic;
      src_i    : in  t_wbs_source_in;
      src_o    : out t_wbs_source_out;
      addr_i   : in  std_logic_vector(g_addr_width-1 downto 0);
      data_i   : in  std_logic_vector(g_data_width-1 downto 0);
      tgd_i    : in  std_logic_vector(g_tgd_width-1 downto 0);
      dvalid_i : in  std_logic;
      error_i  : in  std_logic;
      dreq_o   : out std_logic);
  end component xwb_stream_source;

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

  rst_n <= not(rst_i);

  cmp_stream_sink : xwb_stream_sink
    generic map (
      g_data_width   => c_in_data_width,
      g_addr_width   => g_adr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_input_buffer)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n,
      snk_i    => snk_i,
      snk_o    => snk_o,
      addr_o   => metadata_in(g_adr_width + g_tgd_width-1 downto g_tgd_width),
      data_o   => data_in,
      tgd_o    => metadata_in(g_tgd_width-1 downto 0),
      error_o  => open,
      dvalid_o => valid_input,
      dreq_i   => ce);

  I_in <= data_in(c_in_data_width-1 downto g_input_width);
  Q_in <= data_in(g_input_width-1 downto 0);

  -- this pipeline holds data for signals currently in the integrator stage registers
  ce_valid <= ce and valid_input;
  cmp_integrator_pipe : pipeline
    generic map (
      g_width => g_adr_width + g_tgd_width,
      g_depth => g_stages)
    port map (
      data_i => metadata_in,
      clk_i  => clk_i,
      ce_i   => ce_valid,
      data_o => metadata_middle);

  -- this pipeline holds data for decimated samples. The data is captured from
  -- the the last sample before sampling

  cmp_comb_pipe : pipeline
    generic map (
      g_width => g_adr_width + g_tgd_width,
      g_depth => g_stages + 1)          -- added one to account for the sampler
    port map (
      data_i => metadata_middle,
      clk_i  => clk_i,
      ce_i   => decimation_strobe,
      data_o => metadata_out);

  cmp_cic_decim_I : cic_decim
    generic map (
      DATAIN_WIDTH  => g_input_width,
      DATAOUT_WIDTH => g_output_width,
      M             => g_stages,
      N             => g_delay,
      MAXRATE       => g_decim_rate,
      bitgrowth     => c_bit_growth)
    port map (
      clk_i     => clk_i,
      rst_i     => rst_i,
      en_i      => ce,
      data_i    => I_in,
      data_o    => I_out,
      act_i     => valid_input,
      act_out_i => decimation_strobe,
      val_o     => valid_output);


  cmp_cic_decim_Q : cic_decim
    generic map (
      DATAIN_WIDTH  => g_input_width,
      DATAOUT_WIDTH => g_output_width,
      M             => g_stages,
      N             => g_delay,
      MAXRATE       => g_decim_rate,
      bitgrowth     => c_bit_growth)
    port map (
      clk_i     => clk_i,
      rst_i     => rst_i,
      en_i      => ce,
      data_i    => Q_in,
      data_o    => Q_out,
      act_i     => valid_input,
      act_out_i => decimation_strobe,
      val_o     => open);


  cmp_strobe_gen : strobe_gen
    generic map (
      g_maxrate   => g_decim_rate,
      g_bus_width => c_bus_width)
    port map (
      clock_i  => clk_i,
      reset_i  => rst_i,
      ce_i     => ce_valid,
      ratio_i  => c_ratio_slv,
      strobe_o => decimation_strobe);

  data_out(c_out_data_width-1 downto g_output_width) <= I_out;
  data_out(g_output_width-1 downto 0)                <= Q_out;

  cmp_stream_source : xwb_stream_source
    generic map (
      g_data_width   => c_out_data_width,
      g_addr_width   => g_adr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_output_buffer)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n,
      src_i    => src_i,
      src_o    => src_o,
      addr_i   => metadata_out(g_adr_width + g_tgd_width-1 downto g_tgd_width),
      data_i   => data_out,
      tgd_i    => metadata_out(g_tgd_width-1 downto 0),
      dvalid_i => valid_output,
      error_i  => '0',
      dreq_o   => source_req);

  -- internal registered clock enable, stall if source is full.
  internal_ce : process(clk_i)
  begin
    if rising_edge(clk_i) then
      ce <= ce_i and source_req;
    end if;
  end process;
  
end architecture str;

-------------------------------------------------------------------------------
