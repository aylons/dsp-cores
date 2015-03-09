-------------------------------------------------------------------------------
-- Title      : Testbench for design "cic_dual_wb"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : cic_dual_wb_tb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-11-07
-- Last update: 2015-02-06
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
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

library std;
use std.textio.all;

library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.wb_stream_pkg.all;

-------------------------------------------------------------------------------

entity cic_dual_wb_tb is

end entity cic_dual_wb_tb;

-------------------------------------------------------------------------------

architecture test of cic_dual_wb_tb is

  constant input_freq   : real    := 120.0e6;
  constant clock_period : time    := 1.0 sec /(2.0*input_freq);
  constant ce_period    : natural := 1;
  constant reset_clocks : natural := 8;

  -- component generics

  constant c_input_width   : natural := 24;
  constant c_output_width  : natural := 24;
  constant c_stages        : natural := 1;
  constant c_delay         : natural := 1;
  constant c_decim_rate    : natural := 1000;
  constant c_tgd_width     : natural := c_input_width+1;
  constant c_adr_width     : natural := 2;
  constant c_input_buffer  : natural := 4;
  constant c_output_buffer : natural := 2;

  -- standard signals
  signal clock : std_logic;
  signal rst   : std_logic := '1';
  signal ce    : std_logic := '1';
  signal snk_i : t_wbs_sink_in;
  signal snk_o : t_wbs_sink_out;
  signal src_i : t_wbs_source_in;
  signal src_o : t_wbs_source_out;

  signal tgd_input   : std_logic_vector(c_tgd_width-1 downto 0) := (others => '0');
  signal adr_input   : std_logic_vector(c_adr_width-1 downto 0) := (others => '0');
  signal valid_input : std_logic                                := '0';

  signal tgd_output   : std_logic_vector(c_tgd_width-1 downto 0) := (others => '0');
  signal adr_output   : std_logic_vector(c_adr_width-1 downto 0) := (others => '0');
  signal valid_output : std_logic                                := '0';

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
  
begin
  
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

  uut : cic_dual_wb
    generic map (
      g_input_width   => c_input_width,
      g_output_width  => c_output_width,
      g_stages        => c_stages,
      g_delay         => c_delay,
      g_decim_rate    => c_decim_rate,
      g_tgd_width     => c_tgd_width,
      g_adr_width     => c_adr_width,
      g_input_buffer  => c_input_buffer,
      g_output_buffer => c_output_buffer)
    port map (
      clk_i => clock,
      rst_i => rst,
      ce_i  => ce,
      snk_i => snk_i,
      snk_o => snk_o,
      src_i => src_i,
      src_o => src_o);
  -- waveform generation

  WaveGen : process
    variable count : natural := 0;

    file cic_file       : text open read_mode is "cic.samples";
    variable cur_inline : line;
    variable datain   : real;
    variable data_slv : std_logic_vector(c_input_width-1 downto 0);
    --variable

    variable count_slv, count_tgd_slv : std_logic_vector(c_input_width-1 downto 0) := (others => '0');

    variable I_out : std_logic_vector(c_output_width-1 downto 0) := (others => '0');
    variable Q_out : std_logic_vector(c_output_width-1 downto 0) := (others => '0');

    variable I_out_int, Q_out_int, count_tgd_int : integer;

  begin
    src_i.stall <= '0';

    loop
      wait until clock = '1' and rst = '0';
      if snk_o.stall = '0' then

        count     := count + 1;
        count_slv := std_logic_vector(to_signed(count, c_input_width));

        if not endfile(cic_file) then
          readline(cic_file, cur_inline);

          read(cur_inline, datain);
          data_slv := std_logic_vector(to_signed(integer(datain*real(2**(c_input_width-1))), c_input_width));

          snk_i.dat(c_wbs_data_width-1 downto c_input_width*2) <= (others => '0');
          snk_i.dat(c_input_width*2-1 downto c_input_width)    <= data_slv;
          snk_i.dat(c_input_width-1 downto 0)                  <= data_slv;

          snk_i.adr <= (others => '0');

          snk_i.tgd <= (c_wbs_tgd_width-1 downto c_tgd_width => '0') & count_slv & '0';

          snk_i.stb <= '1';
          snk_i.cyc <= '1';
          
        end if;  -- endfile
      else
        snk_i.stb <= '0';
        snk_i.cyc <= '0';

      end if;

      -- output 
      if (src_o.cyc and src_o.stb) = '1' then

        I_out := src_o.dat(c_output_width*2-1 downto c_output_width);
        Q_out := src_o.dat(c_output_width-1 downto 0);

        count_tgd_slv := src_o.tgd(c_input_width downto 1);
        count_tgd_int := to_integer(signed(count_tgd_slv));

        Q_out_int := to_integer(signed(Q_out));
        I_out_int := to_integer(signed(I_out));

      end if;
    end loop;

  end process WaveGen;


end architecture test;
