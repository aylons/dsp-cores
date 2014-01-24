-------------------------------------------------------------------------------
-- Title      : TOP DSP design for Sirius BPM
-- Project    : 
-------------------------------------------------------------------------------
-- File       : bpm_dsp_example.vhd
-- Author     : Lucas Maziero Russo and Gustavo BM Bruno
-- Company    : 
-- Created    : 2013-12-13
-- Last update: 2014-01-15
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Top design for testing DSP cores for Sirius BPM
-------------------------------------------------------------------------------
-- Copyright (c) 2013 CNPEM
-- Licensed under GNU Lesser General Public License (LGPL) v3.0
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2013-12-13  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity gen_test is
end gen_test;

architecture behaviour of gen_test is

  -- Simulated data from ADC
  signal adc0: std_logic_vector(15 downto 0);
  -- Global Clock Single ended
  signal sys_clk_gen : std_logic := '0';
  signal clk_adc : std_logic;

  -- DDS test
  signal beam_position: std_logic_vector(15 downto 0);
  --Debug data
  signal debug        : std_logic_vector(31 downto 0);

  constant SYS_CLK_PERIOD :time := 5 ns;
  -----------------
  -- Components  --
  -----------------
  component signal_gen is
    generic (
      g_adc_num_bits      : natural := 16;     -- Simulated ADC resolution
      g_carrier_freq      : natural := 20e6;  -- "Carrier" frequency
      g_modulation_db     : real    := -40.0
      );
    port (
      rst : in std_logic;
      clk_adc  : in  std_logic;           -- ADC clock
      beam_position: in std_logic_vector(15 downto 0);
      adc_data : out std_logic_vector(15 downto 0);  -- simulated output data from ADC
      debug: out std_logic_vector(31 downto 0)
      );
  end component;

  component beam_position_gen is
    generic(
      g_resolution : natural := 16;
      g_frequency  : natural := 2e3;
      g_clock      : natural := 130e6
      );
    port(
      rst: in std_logic;
      clk: in std_logic; -- main clock
      beam_position: out std_logic_vector(15 downto 0)
    );
  end component;

  -- Xilinx PLL
  component sys_pll is
    generic(
      -- 200 MHz input clock
      g_divclk_divide  : integer := 4;  --50MHz
      g_clkin_period   : real    := 5.000;
      g_clkbout_mult_f : real    := 13.000; --650MHz

      g_clk0_divide_f : real    := 2.0; --325Mhz
      g_clk1_divide   : integer := 5 --130Mhz
      );      
    port(
      rst_i    : in  std_logic := '0';
      clk_i    : in  std_logic := '0';
      clk0_o   : out std_logic;
      clk1_o   : out std_logic;
      locked_o : out std_logic
      );
  end component;

begin

  -- Obtain core locking and generate necessary clocks
  cmp_sys_pll_inst : sys_pll
    generic map(
      g_divclk_divide  => 4,
      g_clkin_period  => 5.000,
      g_clkbout_mult_f => 13.0,
      g_clk0_divide_f  => 2.0,
      g_clk1_divide    => 5
      )
    port map (
      rst_i    => '0',
      clk_i    => sys_clk_gen,
      clk0_o   => open,
      clk1_o   => clk_adc,  -- 130MHz
      locked_o => open                -- '1' when the PLL has locked
      );

  cmp_signal_gen : signal_gen
    port map(
      rst      => '1',
      clk_adc  => clk_adc,
      adc_data => adc0,
      beam_position => beam_position,
      debug => debug
      );

  cmp_beam_position : beam_position_gen
    port map(
      rst => '1',
      clk => clk_adc,
      beam_position => beam_position
    );

  clk_gen: process is
    constant half_period :time := SYS_CLK_PERIOD / 2;
    begin
      sys_clk_gen <= NOT(sys_clk_gen);
      wait for half_period;
    end process;
    
end behaviour;

