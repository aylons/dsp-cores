-------------------------------------------------------------------------------
-- Title      : Decoupled FIFO
-- Project    : Wishbone stream
-------------------------------------------------------------------------------
-- File       : decoupled_fifo.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-09-18
-- Last update: 2014-09-29
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: FWFT fifo with registers to avoid timing issues between input
-- and output.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-09-18  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;
-------------------------------------------------------------------------------

entity decoupled_fifo is

  generic (
    g_fifo_width : natural := 32;
    g_fifo_depth : natural := 4
    );

  port (
    rst_n_i : in  std_logic;
    clk_i   : in  std_logic;
    d_i     : in  std_logic_vector(g_fifo_width-1 downto 0);
    we_i    : in  std_logic;
    rd_i    : in  std_logic;
    full_o  : out std_logic;
    d_o     : out std_logic_vector(g_fifo_width-1 downto 0);
    valid_o : out std_logic
    );

end entity decoupled_fifo;

-------------------------------------------------------------------------------

architecture str of decoupled_fifo is
  signal rd_d0, rd_cont                      : std_logic := '0';
  signal will_update_out, will_update_middle : std_logic := '0';
  signal q_valid, middle_valid, out_valid    : std_logic := '0';
  signal reg_middle, reg_out                 : std_logic_vector(g_fifo_width-1 downto 0);
  signal fifo_rd                             : std_logic := '0';

  signal fout : std_logic_vector(g_fifo_width-1 downto 0);

begin  -- architecture str

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  cmp_fifo : generic_shiftreg_fifo
    generic map (
      g_data_width => g_fifo_width,
      g_size       => g_fifo_depth
      )
    port map (
      rst_n_i   => rst_n_i,
      clk_i     => clk_i,
      d_i       => d_i,
      we_i      => we_i,
      q_o       => fout,
      rd_i      => fifo_rd,
      full_o    => full_o,
      q_valid_o => q_valid
      );

  -- Below is the ouput logic/registers for the FIFO, inspired by Eli Billauer.
  -- The generic_shiftreg_fifo is already a FWFT fifo, and as such, it may
  -- raise problems with timing, due to a lot of combinatorial logic being
  -- attached to rd_i. As wishbone_stream aims to aid development of
  -- high-speed DSP streams, we decided to employ the technique Billauer
  -- describes at http://billauer.co.il/reg_fifo.html.

  will_update_out    <= (rd_i or not(out_valid)) and (q_valid or middle_valid);
  will_update_middle <= q_valid and out_valid and not(rd_i) and not(middle_valid);
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
        rd_d0        <= '0';
      else

        if (rd_i xor rd_d0) = '1' then
          rd_cont <= rd_i;
        end if;

        rd_d0 <= rd_i;

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
        elsif rd_i = '1' then
          out_valid <= '0';
        end if;

      end if;  -- rst_n_i
    end if;  -- clk
  end process;

  valid_o <= out_valid;
  d_o     <= reg_out;
  
end architecture str;

-------------------------------------------------------------------------------
