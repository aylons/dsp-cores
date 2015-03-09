-------------------------------------------------------------------------------
-- Title      : Wishbone stream source interface
-- Project    : 
-------------------------------------------------------------------------------
-- File       : xwb_stream_source.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-08-12
-- Last update: 2015-02-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Helper component to create a wishbone stream source. Heavily
-- based on xwb_fabric_source.vhd from Tomasz Wlostowski, proposed and first
-- implemented by Lucas Russo <lucas.russo@lnls.br>.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-08-12  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


use work.genram_pkg.all;
use work.wb_stream_pkg.all;

entity xwb_stream_source is
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
    src_i : in  t_wbs_source_in;
    src_o : out t_wbs_source_out;

    -- Decoded & buffered logic
    addr_i   : in  std_logic_vector(g_addr_width-1 downto 0);
    data_i   : in  std_logic_vector(g_data_width-1 downto 0);
    tgd_i    : in  std_logic_vector(g_tgd_width-1 downto 0);
    dvalid_i : in  std_logic;
--    sof_i    : in  std_logic;
--    eof_i    : in  std_logic;
    error_i  : in  std_logic;
    dreq_o   : out std_logic
    );

end xwb_stream_source;

architecture rtl of xwb_stream_source is
  -- FIFO ranges and control bits location
  constant c_data_lsb : natural := 0;
  constant c_data_msb : natural := c_data_lsb + g_data_width - 1;

  constant c_addr_lsb : natural := c_data_msb + 1;
  constant c_addr_msb : natural := c_addr_lsb + g_addr_width - 1;

  constant c_tgd_lsb : natural := c_addr_msb + 1;
  constant c_tgd_msb : natural := c_tgd_lsb + g_tgd_width - 1;

  constant c_fifo_width : integer := c_tgd_msb - c_data_lsb + 1;

  constant c_fifo_depth : integer := g_buffer_depth;

  signal pre_dvalid : std_logic;
  signal pre_eof    : std_logic;

  signal pre_data : std_logic_vector(g_data_width-1 downto 0);
  signal pre_addr : std_logic_vector(g_addr_width-1 downto 0);
  signal pre_tgd  : std_logic_vector(g_tgd_width-1 downto 0);

  signal q_valid, full, almost_full, we, rd, rd_d0, fifo_rd : std_logic := '0';
  signal fin, fout, reg_middle, reg_out                     : std_logic_vector(c_fifo_width-1 downto 0);

  signal rd_cont, will_update_out, will_update_middle : std_logic := '0';
  signal middle_valid, out_valid                      : std_logic := '0';


  signal post_data : std_logic_vector(g_data_width-1 downto 0);
  signal post_adr  : std_logic_vector(g_addr_width-1 downto 0);
  signal post_tgd  : std_logic_vector(g_tgd_width-1 downto 0);

  signal cyc_int : std_logic;

begin  -- rtl

  dreq_o <= not(full);

  we <= (error_i or dvalid_i) and not(full);

  pre_data <= data_i;  -- when (error_i = '0') else f_marshall_wbs_status(err_status);
  pre_addr <= addr_i;
  pre_tgd  <= tgd_i(g_tgd_width-1 downto 1) & (tgd_i(0) or error_i);

  fin <= pre_tgd & pre_addr & pre_data;

  rd <= not src_i.stall;

  cmp_fifo : generic_shiftreg_fifo
    generic map (
      g_data_width => c_fifo_width,
      g_size       => c_fifo_depth
      )
    port map (
      rst_n_i       => rst_n_i,
      clk_i         => clk_i,
      d_i           => fin,
      we_i          => we,
      q_o           => fout,
      rd_i          => rd,
      almost_full_o => almost_full,
      full_o        => full,
      q_valid_o     => q_valid
      );

  -- Below is the ouput logic/registers for the FIFO, inspired by Eli Billauer.
  -- The generic_shiftreg_fifo is already a FWFT fifo, and as such, it may
  -- raise problems with timing, due to a lot of combinatorial logic being
  -- attached to rd_i. As wishbone_stream aims to aid development of
  -- high-speed DSP streams, we decided to employ the technique Billauer
  -- describes at http://billauer.co.il/reg_fifo.html.

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
        -- delayed read

        if (rd xor rd_d0) = '1' then
          rd_cont <= rd;
        end if;
        rd_d0 <= rd;


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


  post_tgd  <= reg_out(c_tgd_msb downto c_tgd_lsb);
  post_data <= reg_out(c_data_msb downto c_data_lsb);
  post_adr  <= reg_out(c_addr_msb downto c_addr_lsb);


  src_o.cyc <= out_valid;
  src_o.stb <= out_valid;               -- and post_dvalid;
  src_o.dat <= (c_wbs_data_width-1 downto g_data_width    => '0') & post_data;
  src_o.adr <= (c_wbs_address_width-1 downto g_addr_width => '0') & post_adr;
  src_o.tgd <= (c_wbs_tgd_width-1 downto g_tgd_width      => '0') & post_tgd;

end rtl;

library ieee;
use ieee.std_logic_1164.all;

use work.wb_stream_pkg.all;

entity wb_stream_source is
  
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

    src_dat_o : out std_logic_vector(c_wbs_data_width-1 downto 0);
    src_adr_o : out std_logic_vector(c_wbs_address_width-1 downto 0);
    src_tgd_o : out std_logic_vector(c_wbs_data_width-1 downto 0);

    src_cyc_o : out std_logic;
    src_stb_o : out std_logic;

    src_stall_i : in std_logic;
    src_ack_i   : in std_logic;

    -- Decoded & buffered fabric
    addr_i    : in  std_logic_vector(g_addr_width-1 downto 0);
    data_i    : in  std_logic_vector(g_data_width-1 downto 0);
    tgd_i     : in  std_logic_vector(g_tgd_width-1 downto 0);
    dvalid_i  : in  std_logic;
    sof_i     : in  std_logic;
    eof_i     : in  std_logic;
    error_i   : in  std_logic;
    bytesel_i : in  std_logic;
    dreq_o    : out std_logic
    );

end wb_stream_source;

architecture wrapper of wb_stream_source is

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
--      sof_i    : in  std_logic;
--      eof_i    : in  std_logic;
      error_i  : in  std_logic;
      dreq_o   : out std_logic);
  end component xwb_stream_source;

  signal src_in  : t_wbs_source_in;
  signal src_out : t_wbs_source_out;

begin  -- wrapper

  xwb_stream_source_1 : entity work.xwb_stream_source
    generic map (
      g_data_width   => g_data_width,
      g_addr_width   => g_addr_width,
      g_tgd_width    => g_tgd_width,
      g_buffer_depth => g_buffer_depth)
    port map (
      clk_i    => clk_i,
      rst_n_i  => rst_n_i,
      src_i    => src_in,
      src_o    => src_out,
      addr_i   => addr_i,
      data_i   => data_i,
      tgd_i    => tgd_i,
      dvalid_i => dvalid_i,
--      sof_i    => sof_i,
--      eof_i    => eof_i,
      error_i  => error_i,
      dreq_o   => dreq_o);

  src_cyc_o <= src_out.cyc;
  src_stb_o <= src_out.stb;
  src_adr_o <= src_out.adr;
  src_dat_o <= src_out.dat;
  src_tgd_o <= src_out.tgd;

  src_in.ack   <= src_ack_i;
  src_in.stall <= src_stall_i;

end wrapper;
