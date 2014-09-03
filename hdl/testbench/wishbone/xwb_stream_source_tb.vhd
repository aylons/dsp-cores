-------------------------------------------------------------------------------
-- Title      : Testbench for design "xwb_stream_source"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : xwb_stream_source_tb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-08-18
-- Last update: 2014-09-02
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-08-18  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity xwb_stream_source_tb is

end entity xwb_stream_source_tb;

-------------------------------------------------------------------------------

architecture testbench of xwb_stream_source_tb is

  constant c_input_freq : real := 120.0e6;
  constant clock_period : time := 1.0 sec / (2.0 * c_input_freq);

  constant c_ce_rate : natural := 4;

  signal reset : std_logic := '1';
  signal clock : std_logic := '0';
  signal rst_n : std_logic;
  signal ce    : std_logic := '0';

  -- component generics
  constant c_data_width   : natural := 32;
  constant c_addr_width   : natural := 4;
  constant c_tgd_width    : natural := 4;
  constant c_buffer_depth : natural := 4;

  -- wishbone stream
  signal src_i : t_wbs_source_in;
  signal src_o : t_wbs_source_out;

  -- source
  signal src_addr  : std_logic_vector(c_addr_width-1 downto 0);
  signal src_data  : std_logic_vector(c_data_width-1 downto 0);
  signal src_tgd   : std_logic_vector(c_tgd_width-1 downto 0);
  signal src_error : std_logic;

  signal src_dvalid : std_logic;
  signal src_sof    : std_logic;
  signal src_eof    : std_logic;

  signal src_dreq : std_logic;

  -- sink
  signal snk_addr  : std_logic_vector(c_addr_width-1 downto 0);
  signal snk_data  : std_logic_vector(c_data_width-1 downto 0);
  signal snk_tgd   : std_logic_vector(c_tgd_width-1 downto 0);
  signal snk_error : std_logic;

  signal snk_dvalid : std_logic;
  signal snk_sof    : std_logic;
  signal snk_eof    : std_logic;

  signal snk_dreq : std_logic;

  -- components

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
      --sof_i    : in  std_logic;
      --eof_i    : in  std_logic;
      error_i  : in  std_logic;
      dreq_o   : out std_logic);
  end component xwb_stream_source;

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
--      sof_o    : out std_logic;
--      eof_o    : out std_logic;
      dreq_i   : in  std_logic);
  end component xwb_stream_sink;
  
begin  -- architecture testbench

  -- clock generation
  clock <= not clock after clock_period;

  rst_gen : process(clock)
    variable clock_count : natural := 4;
  begin
    if rising_edge(clock) then

      if clock_count /= 0 then
        clock_count := clock_count - 1;
      else
        reset <= '0';
      end if;
    end if;
  end process;

  rst_n <= not reset;

  ce_gen : process(clock)
    variable clock_count : natural := c_ce_rate-1;
  begin
    if rising_edge(clock) then
      if clock_count /= 0 then
        ce          <= '0';
        clock_count := clock_count - 1;
      else
        ce          <= '1';
        clock_count := c_ce_rate-1;
      end if;
    end if;
  end process;



  -- waveform generation
  source_read : process
--  variable input_counter : natural := 0;
    file input_file : text open read_mode is "wb_source.samples";

    variable cur_line : line;
    variable data     : std_logic_vector(c_data_width-1 downto 0);
    variable addr     : std_logic_vector(c_addr_width-1 downto 0);
    variable tgd      : std_logic_vector(c_tgd_width-1 downto 0);
    variable err      : std_logic;
  begin

    while not endfile(input_file) loop

      wait until clock = '1';

      if ce = '1' and src_dreq = '1' then

        readline(input_file, cur_line);
        -- Input line has 4 columns
        read(cur_line, data);
        read(cur_line, addr);
        read(cur_line, tgd);
        read(cur_line, err);

        src_data  <= data;
        src_addr  <= addr;
        src_tgd   <= tgd;
        src_error <= err;

        src_dvalid <= '1';
        src_sof    <= '1';
        src_eof    <= '1';
      else

        src_dvalid <= '0';
        src_sof    <= '0';
        src_eof    <= '0';
        src_error  <= '0';

      end if;

    end loop;

    -- file ended, send a tgd
    src_tgd(c_tgd_width-1) <= '1';

    wait until clock = '1' and ce = '1';

    src_dvalid <= '0';
    src_sof    <= '0';
    src_eof    <= '0';
    src_error  <= '0';
    
  end process source_read;

  sink_write : process
    file output_file  : text open write_mode is "wb_sink.samples";
    variable cur_line : line;

    variable data : std_logic_vector(c_data_width-1 downto 0);
    variable addr : std_logic_vector(c_addr_width-1 downto 0);
    variable tgd  : std_logic_vector(c_tgd_width-1 downto 0);
    variable err  : std_logic;
  begin

    loop
      wait until rising_edge(clock);

      if ce = '1' and snk_dvalid = '1' then

        data := snk_data;
        write(cur_line, data); write(cur_line, string'(" "));

        addr := snk_addr;
        write(cur_line, addr); write(cur_line, string'(" "));
        tgd  := snk_tgd;
        write(cur_line, tgd); write(cur_line, string'(" "));
        err  := snk_error;
        write(cur_line, err);

        writeline(output_file, cur_line);
      end if;

      exit when tgd(c_tgd_width-1) = '1';
      
    end loop;
    assert (false) report "Test finished" severity failure;

  end process;

-- component instantiation
  DUT_source : xwb_stream_source
    generic map (
      g_data_width   => c_data_width,
      g_addr_width   => c_addr_width,
      g_tgd_width    => c_tgd_width,
      g_buffer_depth => c_buffer_depth)
    port map (
      clk_i    => clock,
      rst_n_i  => rst_n,
      src_i    => src_i,
      src_o    => src_o,
      addr_i   => src_addr,
      data_i   => src_data,
      tgd_i    => src_tgd,
      dvalid_i => src_dvalid,
      --sof_i    => src_sof,
      --eof_i    => src_eof,
      error_i  => src_error,
      dreq_o   => src_dreq);

  snk_dreq <= ce;

  DUT_sink : xwb_stream_sink
    generic map (
      g_data_width   => c_data_width,
      g_addr_width   => c_addr_width,
      g_tgd_width    => c_tgd_width,
      g_buffer_depth => c_buffer_depth)
    port map (
      clk_i    => clock,
      rst_n_i  => rst_n,
      snk_i    => src_o,
      snk_o    => src_i,
      addr_o   => snk_addr,
      data_o   => snk_data,
      tgd_o    => snk_tgd,
      error_o  => snk_error,
      dvalid_o => snk_dvalid,
--      sof_o    => snk_sof,
--      eof_o    => snk_eof,
      dreq_i   => snk_dreq);

end architecture testbench;
