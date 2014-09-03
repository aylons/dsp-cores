-------------------------------------------------------------------------------
-- Title      : Wishbone Packet Fabric buffered packet sink
-- Project    : WR Cores Collection
-------------------------------------------------------------------------------
-- File       : xwb_fabric_sink.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2012-01-16
-- Last update: 2014-09-02
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: A simple WB packet streaming sink with builtin FIFO buffer.
-- Outputs a trivial interface (start-of-packet, end-of-packet, data-valid)
-------------------------------------------------------------------------------
--
-- Copyright (c) 2011 CERN
--
-- This source file is free software; you can redistribute it
-- and/or modify it under the terms of the GNU Lesser General
-- Public License as published by the Free Software Foundation;
-- either version 2.1 of the License, or (at your option) any
-- later version.
--
-- This source is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU Lesser General Public License for more
-- details.
--
-- You should have received a copy of the GNU Lesser General
-- Public License along with this source; if not, download it
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-01-16  1.0      twlostow        Created
-------------------------------------------------------------------------------
-- Modified by Lucas Russo <lucas.russo@lnls.br>
-- Modified by Gustavo Bruno <gustavo.bruno@lnls.br>

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;
use work.wb_stream_pkg.all;

entity xwb_stream_sink is
  generic(
    g_data_width   : natural := 32;
    g_addr_width   : natural := 4;
    g_tgd_width    : natural := 4;
    g_buffer_depth : natural := 4
    );

  port (
    clk_i   : in std_logic;
    rst_n_i : in std_logic;

    -- Wishbone Fabric Interface I/O
    snk_i : in  t_wbs_sink_in;
    snk_o : out t_wbs_sink_out;

    -- Decoded & buffered fabric
    addr_o   : out std_logic_vector(g_addr_width-1 downto 0);
    data_o   : out std_logic_vector(g_data_width-1 downto 0);
    tgd_o    : out std_logic_vector(g_tgd_width-1 downto 0);
    error_o  : out std_logic;
    dvalid_o : out std_logic;
    dreq_i   : in  std_logic
    );

end xwb_stream_sink;

architecture rtl of xwb_stream_sink is
  -- FIFO ranges
  constant c_data_lsb : natural := 0;
  constant c_data_msb : natural := c_data_lsb + g_data_width - 1;

  constant c_addr_lsb : natural := c_data_msb + 1;
  constant c_addr_msb : natural := c_addr_lsb + g_addr_width - 1;

  constant c_tgd_lsb : natural := c_addr_msb + 1;
  constant c_tgd_msb : natural := c_tgd_lsb + g_tgd_width - 1;

  constant c_fifo_width : integer := c_tgd_msb - c_data_lsb + 1;
  constant c_fifo_depth : integer := g_buffer_depth;

  signal q_valid, full, we, rd, fifo_rd : std_logic := '0';

  signal fin, fout, reg_middle, reg_out : std_logic_vector(c_fifo_width-1 downto 0);
  signal cyc_d0, rd_d0                  : std_logic;

  signal pre_eof, pre_sof : std_logic;

  signal post_addr : std_logic_vector(g_addr_width-1 downto 0);
  signal post_data : std_logic_vector(g_data_width-1 downto 0);
  signal post_tgd  : std_logic_vector(g_tgd_width-1 downto 0);

  signal rd_cont, will_update_out, will_update_middle : std_logic := '0';
  signal middle_valid, out_valid                      : std_logic := '0';

  signal snk_out : t_wbs_sink_out;

begin  -- rtl

  p_delay_cyc_and_rd : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        cyc_d0 <= '0';
        rd_d0  <= '0';
      else
        if(full = '0') then
          cyc_d0 <= snk_i.cyc;
        end if;

        rd_d0 <= rd;
      end if;
    end if;
  end process;

  pre_sof <= snk_i.cyc and not cyc_d0;  -- sof
  pre_eof <= not snk_i.cyc and cyc_d0;  -- eof

  -- FIXME: Should this be part of the packet?
--  pre_dvalid <= snk_i.stb and snk_i.cyc and not snk_out.stall;  -- data valid

  -- Bus is at least as large as actual data. Exceeding bits must be pruned to
  -- avoid waste of buffer space.
  fin(c_data_msb downto c_data_lsb) <= snk_i.dat(g_data_width-1 downto 0);
  fin(c_addr_msb downto c_addr_lsb) <= snk_i.adr(g_addr_width-1 downto 0);
  fin(c_tgd_msb downto c_tgd_lsb)   <= snk_i.tgd(g_tgd_width-1 downto 0);

--  fin(c_logic_msb downto c_logic_lsb) <= pre_sof & pre_eof & pre_dvalid;

  p_gen_ack : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        snk_out.ack <= '0';
      else
        snk_out.ack <= snk_i.cyc and snk_i.stb and not snk_out.stall;
      end if;
    end if;
  end process;

  -- Stall in the first cycle of a frame
  snk_out.stall <= full;

  snk_o <= snk_out;
 
  -- FIX. Review the comparison fin(c_logic_msb downto c_logic_lsb) /= c_logic_zeros
  we <= '1' when (snk_i.cyc = '1' and snk_i.stb = '1' and full = '0') else '0';


  cmp_fifo : generic_shiftreg_fifo
    generic map (
      g_data_width => c_fifo_width,
      g_size       => c_fifo_depth
      )
    port map (
      rst_n_i   => rst_n_i,
      clk_i     => clk_i,
      d_i       => fin,
      we_i      => we,
      q_o       => fout,
      rd_i      => fifo_rd,
      full_o    => full,
      q_valid_o => q_valid
      );

  -- Below is the ouput logic/registers for the FIFO, inspired by Eli Billauer.
  -- The generic_shiftreg_fifo is already a FWFT fifo, and as such, it may
  -- raise problems with timing, due to a lot of combinatorial logic being
  -- attached to rd_i. As wishbone_stream aims to aid development of
  -- high-speed DSP streams, we decided to employ the technique Billauer
  -- describes at http://billauer.co.il/reg_fifo.html.

  rd <= dreq_i;
  will_update_out    <= (rd or not(out_valid)) and (q_valid or middle_valid);
  will_update_middle <= q_valid and out_valid and not(rd) and not(middle_valid);
  fifo_rd            <= q_valid and (rd_cont or not(out_valid) or not(middle_valid));

  p_fout_reg : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        reg_middle   <= (others => '0');
        reg_out      <= (others => '0');
        middle_valid <= '0';
        out_valid    <= '0';
        rd_cont      <= '0';
      else

        if (rd xor rd_d0) = '1' then
          rd_cont <= rd;
        end if;

        if will_update_middle = '1' then
          reg_middle <= fout;
        end if;

        if will_update_out = '1' then
          if middle_valid = '1' then
            reg_out <= reg_middle;
          else
            reg_out <= fout;
          end if;
        end if;

        if will_update_middle = '1' then
          middle_valid <= '1';
        elsif will_update_out = '1' then
          middle_valid <= '0';
        end if;

        if will_update_out = '1' then
          out_valid <= '1';
        elsif rd = '1' then
          out_valid <= '0';
        end if;
        
      end if;  -- rst_n_i
    end if;  -- clk
  end process;

--  post_sof    <= reg_out(c_sof_bit);    -- and rd_d0 and q_valid;
--  post_dvalid <= out_valid;
--  post_eof    <= reg_out(c_eof_bit);
  post_data <= reg_out(c_data_msb downto c_data_lsb);
  post_addr <= reg_out(c_addr_msb downto c_addr_lsb);
  post_tgd  <= reg_out(c_tgd_msb downto c_tgd_lsb);

--  sof_o    <= post_sof and rd;
  dvalid_o <= out_valid;
  error_o  <= post_tgd(c_TGD_ERROR_BIT);

--  eof_o  <= post_eof and rd;
  data_o <= post_data;
  addr_o <= post_addr;
  tgd_o  <= post_tgd;

end rtl;

library ieee;
use ieee.std_logic_1164.all;

use work.genram_pkg.all;
use work.wb_stream_pkg.all;

entity wb_stream_sink is

  generic (
    g_data_width   : natural;
    g_addr_width   : natural;
    g_tgd_width    : natural;
    g_buffer_depth : natural);
  port (
    clk_i     : in std_logic;
    rst_n_i   : in std_logic;
    snk_dat_i : in std_logic_vector(g_data_width-1 downto 0);
    snk_adr_i : in std_logic_vector(g_addr_width-1 downto 0);
    snk_tgd_i : in std_logic_vector(g_tgd_width-1 downto 0);
    snk_cyc_i : in std_logic;
    snk_stb_i : in std_logic;

    snk_stall_o : out std_logic;
    snk_ack_o   : out std_logic;

    addr_o   : out std_logic_vector(g_addr_width-1 downto 0);
    data_o   : out std_logic_vector(g_data_width-1 downto 0);
    tgd_o    : out std_logic_vector(g_tgd_width-1 downto 0);
    error_o  : out std_logic;
    dvalid_o : out std_logic;
    sof_o    : out std_logic;
    eof_o    : out std_logic;
    dreq_i   : in  std_logic);
end wb_stream_sink;

architecture wrapper of wb_stream_sink is

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

  signal snk_in  : t_wbs_sink_in;
  signal snk_out : t_wbs_sink_out;

begin  -- wrapper

  xwb_stream_sink_1 : entity work.xwb_stream_sink
    generic map (
      g_data_width   => g_data_width,
      g_addr_width   => g_addr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_buffer_depth)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n_i,
      snk_i    => snk_in,
      snk_o    => snk_out,
      addr_o   => addr_o,
      data_o   => data_o,
      tgd_o    => tgd_o,
      error_o  => error_o,
      dvalid_o => dvalid_o,
--      sof_o    => sof_o,
--      eof_o    => eof_o,
      dreq_i   => dreq_i);

  snk_in.adr <= snk_adr_i;
  snk_in.dat <= snk_dat_i;
  snk_in.tgd <= snk_tgd_i;
  snk_in.cyc <= snk_cyc_i;
  snk_in.stb <= snk_stb_i;

  snk_stall_o <= snk_out.stall;
  snk_ack_o   <= snk_out.ack;

end wrapper;
