-------------------------------------------------------------------------------
-- Title      : Wishbone stream multiplexer
-- Project    : 
-------------------------------------------------------------------------------
-- File       : wb_mux.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-11-08
-- Last update: 2015-01-13
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: This module multiplex several wishbone stream fluxes into a
-- single flux, address by the addr bus. The address for each flux may be
-- already defined, defined on arrival, or both, in which case the new address
-- will be appended as MSBs to the old address.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-11-08  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use ieee.math_real.all;

use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity wb_mux is

  generic (
    g_input_number : natural := 4;

    g_dat_width    : natural := 32;
    g_tgd_width    : natural := 4;
    g_adr_in_width : natural := 2;      -- output will have
                                        -- log2(g_input_number) more bits

    g_input_buffer  : natural := 2;
    g_output_buffer : natural := 6
    );

  port (
    clk_i : in std_logic;
    rst_i : in std_logic;

    snk_i : in  t_wbs_sink_in_array(g_input_number-1 downto 0);
    snk_o : out t_wbs_sink_out_array(g_input_number-1 downto 0);
    src_i : in  t_wbs_source_in;
    src_o : out t_wbs_source_out);

end entity wb_mux;

-------------------------------------------------------------------------------

architecture str of wb_mux is

  -- signals for input array
  type t_wbs_payload is record
    adr   : std_logic_vector(g_adr_in_width-1 downto 0);
    tgd   : std_logic_vector(g_tgd_width-1 downto 0);
    dat   : std_logic_vector(g_dat_width-1 downto 0);
    valid : std_logic;
  end record;

  type t_payload_array is array (natural range <>) of t_wbs_payload;

  signal data_array : t_payload_array(g_input_number-1 downto 0);

  type t_req_array is array (natural range <>) of std_logic;

  signal dreq : t_req_array(g_input_number-1 downto 0);

  --signals for multiplexer
  constant c_new_adr_width : natural := natural(ceil(log2(real(g_input_number))));
  constant c_adr_width     : natural := c_new_adr_width + g_adr_in_width;

  signal cur_slot : natural range 0 to g_input_number-1;

  signal cur_adr   : std_logic_vector(c_adr_width-1 downto 0);
  signal cur_dat   : std_logic_vector(g_dat_width-1 downto 0);
  signal cur_tgd   : std_logic_vector(g_tgd_width-1 downto 0);
  signal cur_valid : std_logic;
  signal src_req   : std_logic;


  signal rst_n : std_logic;

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

begin  -- architecture str

  rst_n <= not(rst_i);

  gen_input : for j in 0 to g_input_number-1 generate

    cmp_sink : xwb_stream_sink
      generic map (
        g_data_width   => g_dat_width,
        g_addr_width   => g_adr_in_width,
        g_tgd_width    => g_tgd_width,
        g_buffer_depth => g_input_buffer)
      port map (
        clk_i    => clk_i,
        rst_n_i  => rst_n,
        snk_i    => snk_i(j),
        snk_o    => snk_o(j),
        addr_o   => data_array(j).adr,
        data_o   => data_array(j).dat,
        tgd_o    => data_array(j).tgd,
        error_o  => open,
        dvalid_o => data_array(j).valid,
        dreq_i   => dreq(j));

  end generate gen_input;

  -- counter to cycle between inputs
  p_tdm_counter : process(clk_i)
  begin

    if rising_edge(clk_i) then

      if rst_i = '1' then
        cur_slot <= 0;
      else

        if cur_slot = g_input_number-1 then
          cur_slot <= 0;
        else
          cur_slot <= cur_slot + 1;
        end if;
      end if;  --rst
    end if;  --clk
    
  end process p_tdm_counter;

  --output stage (multiplexing occurs at data input)

  cur_adr <= std_logic_vector(to_unsigned(cur_slot, c_new_adr_width)) &
             data_array(cur_slot).adr;
  cur_dat   <= data_array(cur_slot).dat;
  cur_tgd   <= data_array(cur_slot).tgd;
  cur_valid <= data_array(cur_slot).valid;

  p_req_demux : process(clk_i)
  begin

--    if rising_edge(clk_i) then

    for j in 0 to g_input_number-1 loop
      
      if(j = cur_slot) then
        dreq(j) <= src_req;
      else
        dreq(j) <= '0';
      end if;

    end loop;
--    end if;  --rising_edge
  end process;

  cmp_wbs_source : xwb_stream_source
    generic map (
      g_data_width   => g_dat_width,
      g_addr_width   => c_adr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_output_buffer)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n,
      src_i    => src_i,
      src_o    => src_o,
      addr_i   => cur_adr,
      data_i   => cur_dat,
      tgd_i    => cur_tgd,
      dvalid_i => cur_valid,
      error_i  => '0',
      dreq_o   => src_req);

end architecture str;

-------------------------------------------------------------------------------
