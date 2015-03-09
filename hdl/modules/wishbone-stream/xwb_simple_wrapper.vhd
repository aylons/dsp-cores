-------------------------------------------------------------------------------
-- Title      : Wishbone stream simple wrapper for data processors
-- Project    : 
-------------------------------------------------------------------------------
-- File       : xwb_simple_wrapper.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-10-08
-- Last update: 2015-02-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: This is a component that mus be connected to both the input and
-- output (ie, it "wraps") of a data processor to make it wishbone stream compliant.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-10-08  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


use work.genram_pkg.all;
use work.wb_stream_pkg.all;


entity xwb_simple_wrapper is

  generic (
    g_input_width   : natural := 32;
    g_output_width  : natural := 32;
    g_tgd_width     : natural := 4;
    g_adr_width     : natural := 4;
    g_input_buffer  : natural := 4;
    g_output_buffer : natural := 2;
    g_simultaneous  : natural := 4
    );

  port (
    -- facing the outside
    clk_i : in  std_logic;
    rst_i : in  std_logic;
    ce_i  : in  std_logic;
    snk_i : in  t_wbs_sink_in;
    snk_o : out t_wbs_sink_out;
    src_i : in  t_wbs_source_in;
    src_o : out t_wbs_source_out;

    -- facing the inside
    ce_o : out std_logic;

    data_o : out std_logic_vector(g_input_width-1 downto 0);
    data_i : in  std_logic_vector(g_output_width-1 downto 0);

    valid_o : out std_logic;
    valid_i : in  std_logic;

    error_o : out std_logic;
    error_i : in  std_logic
    );

end entity xwb_simple_wrapper;

architecture str of xwb_simple_wrapper is

  signal ce, cei_d0 : std_logic;

  signal tgd_sink, tgd_source           : std_logic_vector(g_tgd_width-1 downto 0)                 := (others => '0');
  signal adr_sink, adr_source           : std_logic_vector(g_adr_width-1 downto 0)                 := (others => '0');
  signal metadata_sink, metadata_source : std_logic_vector(g_tgd_width + g_adr_width - 1 downto 0) := (others => '0');

  signal valid_sink, valid_source : std_logic := '0';
  signal ack_sink, ack_source     : std_logic := '0';
  signal source_req, full_meta    : std_logic := '0';
  signal rst_n                    : std_logic;


  component decoupled_fifo is
    generic (
      g_fifo_width : natural;
      g_fifo_depth : natural);
    port (
      rst_n_i : in  std_logic;
      clk_i   : in  std_logic;
      d_i     : in  std_logic_vector(g_fifo_width-1 downto 0);
      we_i    : in  std_logic;
      rd_i    : in  std_logic;
      full_o  : out std_logic;
      d_o     : out std_logic_vector(g_fifo_width-1 downto 0);
      valid_o : out std_logic);
  end component decoupled_fifo;

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
      addr_o   : out std_logic_vector(g_adr_width-1 downto 0);
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
      addr_i   : in  std_logic_vector(g_adr_width-1 downto 0);
      data_i   : in  std_logic_vector(g_data_width-1 downto 0);
      tgd_i    : in  std_logic_vector(g_tgd_width-1 downto 0);
      dvalid_i : in  std_logic;
      error_i  : in  std_logic;
      dreq_o   : out std_logic);
  end component xwb_stream_source;

begin

  rst_n <= not(rst_i);

  cmp_wb_sink : xwb_stream_sink
    generic map (
      g_data_width   => g_input_width,
      g_addr_width   => g_adr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_input_buffer)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n,
      snk_i    => snk_i,
      snk_o    => snk_o,
      addr_o   => adr_sink,
      data_o   => data_o,
      tgd_o    => tgd_sink,
      error_o  => error_o,
      dvalid_o => valid_sink,
      dreq_i   => ack_sink);

  valid_o <= ack_sink;

  -- Metadata
  metadata_sink <= tgd_sink & adr_sink;
  ack_sink      <= not(full_meta) and ce and valid_sink;
  ack_source    <= ce and valid_i;

  cmp_metadata : decoupled_fifo
    generic map(
      g_fifo_width => g_adr_width + g_tgd_width,
      g_fifo_depth => g_simultaneous)
    port map (
      rst_n_i => rst_n,
      clk_i   => clk_i,
      d_i     => metadata_sink,
      we_i    => ack_sink,
      rd_i    => ack_source,
      d_o     => metadata_source,
      full_o  => full_meta);

  tgd_source <= metadata_source(g_tgd_width + g_adr_width - 1 downto g_adr_width);
  adr_source <= metadata_source(g_adr_width - 1 downto 0);

  cmp_wb_source : xwb_stream_source
    generic map (
      g_data_width   => g_output_width,
      g_addr_width   => g_adr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_output_buffer)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n,
      src_i    => src_i,
      src_o    => src_o,
      addr_i   => adr_source,
      data_i   => data_i,
      tgd_i    => tgd_source,
      dvalid_i => ack_source,
      error_i  => error_i,
      dreq_o   => source_req);

  ce_gen : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' then
        cei_d0 <= '0';
      else
        cei_d0 <= ce_i;
      end if;
    end if;
  end process;

  ce <= cei_d0 and source_req;
  ce_o <= ce;
  
end architecture str;
