-------------------------------------------------------------------------------
-- Title      : Testbench for design "wb_mux"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : wb_mux_tb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-11-18
-- Last update: 2015-01-13
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-11-18  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity wb_mux_tb is

end entity wb_mux_tb;

-------------------------------------------------------------------------------

architecture test of wb_mux_tb is

  constant input_freq   : real    := 120.0e6;
  constant clock_period : time    := 1.0 sec /(2.0*input_freq);
  constant ce_period    : natural := 1;
  constant reset_clocks : natural := 8;

  -- component generics
  constant c_input_number  : natural := 4;
  constant c_dat_width     : natural := 32;
  constant c_tgd_width     : natural := 32;
  constant c_adr_in_width  : natural := 2;
  constant c_adr_out_width : natural := 4;
  constant c_input_buffer  : natural := 2;
  constant c_output_buffer : natural := 2;

  -- component ports
  signal clock : std_logic;
  signal rst   : std_logic;
  signal ce    : std_logic;
  signal snk_i : t_wbs_sink_in_array(c_input_number-1 downto 0);
  signal snk_o : t_wbs_sink_out_array(c_input_number-1 downto 0);
  signal src_i : t_wbs_source_in;
  signal src_o : t_wbs_source_out;

  signal dat_out : std_logic_vector(c_dat_width-1 downto 0);
  signal tgd_out : std_logic_vector(c_tgd_width-1 downto 0);
  signal adr_out : std_logic_vector(c_adr_out_width-1 downto 0);

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

begin  -- architecture test

  clk_gen : process
  begin
    clock <= '0';
    wait for clock_period;
    clock <= '1';
    wait for clock_period;
  end process;

  rst_gen : process(clock)
    variable rst_count : natural := reset_clocks;
  begin
    if rising_edge(clock) then
      if rst_count > 0 then
        rst_count := rst_count-1;
      else
        rst <= '0';
      end if;
    end if;
  end process rst_gen;

  ce_gen : process(clock)
    variable ce_count : natural := ce_period;
  begin
    if rising_edge(clock) then
      ce_count := ce_count-1;
      if ce_count = 0 then
        ce       <= '1';
        ce_count := ce_period;
      else
        ce <= '0';
      end if;
    end if;
  end process;

  -- component instantiation
  uut : wb_mux
    generic map (
      g_input_number  => c_input_number,
      g_dat_width     => c_dat_width,
      g_tgd_width     => c_tgd_width,
      g_adr_in_width  => c_adr_in_width,
      g_input_buffer  => c_input_buffer,
      g_output_buffer => c_output_buffer)
    port map (
      clk_i => clock,
      rst_i => rst,
      snk_i => snk_i,
      snk_o => snk_o,
      src_i => src_i,
      src_o => src_o);

  -- waveform generation
  WaveGen : process
    type t_natural_array is array (natural range <>) of natural;

    variable count : t_natural_array(c_input_number-1 downto 0) := (others => 0);

  begin

    src_i.stall <= '0';

    loop

      -- cycle in counts of 100 with added j to follow the data.
      wait until clock = '1' and rst = '0';

      for j in 0 to c_input_number-1 loop

        if snk_o(j).stall = '0' then

          count(j) := count(j) + 100;

          snk_i(j).dat <= std_logic_vector(to_unsigned(count(j)+j, c_wbs_data_width));
          snk_i(j).tgd <= std_logic_vector(to_unsigned(count(j)+j, c_wbs_tgd_width));
          snk_i(j).adr <= (others => '0');

          snk_i(j).stb <= '1';
          snk_i(j).cyc <= '1';

        else
          snk_i(j).stb <= '0';
          snk_i(j).cyc <= '0';

        end if;
      end loop;  -- loop j

      -- output 
      if (src_o.cyc and src_o.stb) = '1' then

        dat_out <= src_o.dat(c_dat_width-1 downto 0);
        tgd_out <= src_o.tgd(c_tgd_width-1 downto 0);
        adr_out <= src_o.adr(c_adr_out_width-1 downto 0);
        
      end if;
    end loop;


  end process WaveGen;



end architecture test;
