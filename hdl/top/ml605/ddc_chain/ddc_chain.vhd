-------------------------------------------------------------------------------
-- Title      : DSP-core DDC chain
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ddc_chain.vhd
-- Author     : Gustavo BM Bruno
-- Company    : 
-- Created    : 2014-01-30
-- Last update: 2014-02-21
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: DDC chain down to the FOFB rate, with DSC
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-01-30  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity ddc_chain is
  generic(
    g_clk_freq : real := 120.0e6;
    g_sw_freq  : real := 100.0e3);
  port(
    signal reset_n_i : in  std_logic;
    signal clk_adc   : in  std_logic;
    signal adc_data  : in  std_logic_vector(15 downto 0);
    signal amplitude : out std_logic_vector(31 downto 0);
    signal tvalid    : out std_logic;   -- AXI4 stream valid
    signal switch_o  : out std_logic
    );

end entity ddc_chain;

architecture structural of ddc_chain is

  signal conditioned_data : std_logic_vector(23 downto 0);
  signal I_sig            : std_logic_vector(31 downto 0);
  signal Q_sig            : std_logic_vector(31 downto 0);
  signal mixer_valid      : std_logic;

  signal phase_out : std_logic_vector(31 downto 0);  -- phase of the signal, useless for now

  component input_conditioner is
    generic (
      g_clk_freq : real;
      g_sw_freq  : real);
    port (
      reset_n_i     : in  std_logic;
      clk_i         : in  std_logic;
      adc_data_i    : in  std_logic_vector(15 downto 0);
      switch_o      : out std_logic;
      data_output_o : out std_logic_vector(23 downto 0));
  end component input_conditioner;

  component mixer is
    port (
      reset_n_i : in  std_logic;
      clk     : in  std_logic;
      input   : in  std_logic_vector(23 downto 0);
      I_out   : out std_logic_vector(31 downto 0);
      Q_out   : out std_logic_vector(31 downto 0);
      tvalid  : out std_logic);
  end component mixer;

  component cordic_v5_0 is
    port (
      aclk                    : in  std_logic;
      s_axis_cartesian_tvalid : in  std_logic;
      s_axis_cartesian_tdata  : in  std_logic_vector(63 downto 0);
      m_axis_dout_tvalid      : out std_logic;
      m_axis_dout_tdata       : out std_logic_vector(63 downto 0));
  end component cordic_v5_0;
  
begin

  cmp_conditioner : input_conditioner
    generic map (
      g_clk_freq => g_clk_freq,
      g_sw_freq  => g_sw_freq)
    port map (
      reset_n_i     => reset_n_i,
      clk_i         => clk_adc,
      adc_data_i    => adc_data,
      switch_o      => switch_o,
      data_output_o => conditioned_data);

  cmp_mixer : mixer
    port map(
      reset_n_i => reset_n_i,
      clk     => clk_adc,
      input   => conditioned_data,
      I_out   => I_sig,
      Q_out   => Q_sig,
      tvalid  => mixer_valid);

  cmp_cordic : cordic_v5_0
    port map (
      aclk                                 => clk_adc,
      s_axis_cartesian_tvalid              => mixer_valid,
      s_axis_cartesian_tdata(63 downto 32) => Q_sig,
      s_axis_cartesian_tdata(31 downto 0)  => I_sig,
      m_axis_dout_tvalid                   => tvalid,
      m_axis_dout_tdata(63 downto 32)      => phase_out,
      m_axis_dout_tdata(31 downto 0)       => amplitude);

end structural;
