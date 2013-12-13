------------------------------------------------------------------------------
-- Title      : Wishbone Position Calculation Core
------------------------------------------------------------------------------
-- Author     : Lucas Maziero Russo
-- Company    : CNPEM LNLS-DIG
-- Created    : 2013-07-02
-- Platform   : FPGA-generic
-------------------------------------------------------------------------------
-- Description: Core Module for position calculation with de-cross, amplitude compensation
-- and delay tuning.
-------------------------------------------------------------------------------
-- Copyright (c) 2012 CNPEM
-- Licensed under GNU Lesser General Public License (LGPL) v3.0
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2013-07-02  1.0      lucas.russo        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.vcomponents.all;

library work;
-- Main Wishbone Definitions
use work.wishbone_pkg.all;
-- DSP Cores
use work.dsp_cores_pkg.all;
-- Position Calc
use work.position_calc_core_pkg.all;

entity wb_position_calc_core is
generic
(
  g_interface_mode                          : t_wishbone_interface_mode      := CLASSIC;
  g_address_granularity                     : t_wishbone_address_granularity := WORD;
  g_with_switching                          : natural := 0
);
port
(
  rst_n_i                                   : in std_logic;
  clk_i                                     : in std_logic; -- Wishbone clock
  fs_rst_n_i                                : in std_logic; -- FS reset
  fs_clk_i                                  : in std_logic; -- clock period = 8.8823218389287 ns (112.583175675676 Mhz)
  fs_clk2x_i                                : in std_logic; -- clock period = 4.4411609194644 ns (225.166351351351 Mhz)

  -----------------------------
  -- Wishbone signals
  -----------------------------

  wb_adr_i                                  : in  std_logic_vector(c_wishbone_address_width-1 downto 0) := (others => '0');
  wb_dat_i                                  : in  std_logic_vector(c_wishbone_data_width-1 downto 0) := (others => '0');
  wb_dat_o                                  : out std_logic_vector(c_wishbone_data_width-1 downto 0);
  wb_sel_i                                  : in  std_logic_vector(c_wishbone_data_width/8-1 downto 0) := (others => '0');
  wb_we_i                                   : in  std_logic := '0';
  wb_cyc_i                                  : in  std_logic := '0';
  wb_stb_i                                  : in  std_logic := '0';
  wb_ack_o                                  : out std_logic;
  wb_stall_o                                : out std_logic;

  -----------------------------
  -- Raw ADC signals
  -----------------------------

  adc_ch0_i                                 : in std_logic_vector(15 downto 0);
  adc_ch1_i                                 : in std_logic_vector(15 downto 0);
  adc_ch2_i                                 : in std_logic_vector(15 downto 0);
  adc_ch3_i                                 : in std_logic_vector(15 downto 0);

  -----------------------------
  -- DSP config parameter signals
  -----------------------------

  del_sig_div_fofb_thres_i                  : in std_logic_vector(25 downto 0);
  del_sig_div_monit_thres_i                 : in std_logic_vector(25 downto 0);
  del_sig_div_tbt_thres_i                   : in std_logic_vector(25 downto 0);

  ksum_i                                    : in std_logic_vector(24 downto 0);
  kx_i                                      : in std_logic_vector(24 downto 0);
  ky_i                                      : in std_logic_vector(24 downto 0);

  dds_config_valid_ch0_i                    : in std_logic;
  dds_config_valid_ch1_i                    : in std_logic;
  dds_config_valid_ch2_i                    : in std_logic;
  dds_config_valid_ch3_i                    : in std_logic;
  dds_pinc_ch0_i                            : in std_logic_vector(29 downto 0);
  dds_pinc_ch1_i                            : in std_logic_vector(29 downto 0);
  dds_pinc_ch2_i                            : in std_logic_vector(29 downto 0);
  dds_pinc_ch3_i                            : in std_logic_vector(29 downto 0);
  dds_poff_ch0_i                            : in std_logic_vector(29 downto 0);
  dds_poff_ch1_i                            : in std_logic_vector(29 downto 0);
  dds_poff_ch2_i                            : in std_logic_vector(29 downto 0);
  dds_poff_ch3_i                            : in std_logic_vector(29 downto 0);

  -----------------------------
  -- Position calculation at various rates
  -----------------------------

  adc_ch0_dbg_data_o                        : out std_logic_vector(15 downto 0);
  adc_ch1_dbg_data_o                        : out std_logic_vector(15 downto 0);
  adc_ch2_dbg_data_o                        : out std_logic_vector(15 downto 0);
  adc_ch3_dbg_data_o                        : out std_logic_vector(15 downto 0);

  bpf_ch0_o                                 : out std_logic_vector(23 downto 0);
  bpf_ch1_o                                 : out std_logic_vector(23 downto 0);
  bpf_ch2_o                                 : out std_logic_vector(23 downto 0);
  bpf_ch3_o                                 : out std_logic_vector(23 downto 0);
  bpf_valid_o                               : out std_logic;

  mix_ch0_i_o                               : out std_logic_vector(23 downto 0);
  mix_ch0_q_o                               : out std_logic_vector(23 downto 0);
  mix_ch1_i_o                               : out std_logic_vector(23 downto 0);
  mix_ch1_q_o                               : out std_logic_vector(23 downto 0);
  mix_ch2_i_o                               : out std_logic_vector(23 downto 0);
  mix_ch2_q_o                               : out std_logic_vector(23 downto 0);
  mix_ch3_i_o                               : out std_logic_vector(23 downto 0);
  mix_ch3_q_o                               : out std_logic_vector(23 downto 0);
  mix_valid_o                               : out std_logic;

  tbt_decim_ch0_i_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch0_q_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch1_i_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch1_q_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch2_i_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch2_q_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch3_i_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_ch3_q_o                         : out std_logic_vector(23 downto 0);
  tbt_decim_valid_o                         : out std_logic;

  tbt_decim_q_ch01_incorrect_o              : out std_logic;
  tbt_decim_q_ch23_incorrect_o              : out std_logic;

  tbt_amp_ch0_o                             : out std_logic_vector(23 downto 0);
  tbt_amp_ch0_valid_o                       : out std_logic;
  tbt_amp_ch1_o                             : out std_logic_vector(23 downto 0);
  tbt_amp_ch1_valid_o                       : out std_logic;
  tbt_amp_ch2_o                             : out std_logic_vector(23 downto 0);
  tbt_amp_ch2_valid_o                       : out std_logic;
  tbt_amp_ch3_o                             : out std_logic_vector(23 downto 0);
  tbt_amp_ch3_valid_o                       : out std_logic;

  tbt_pha_ch0_o                             : out std_logic_vector(23 downto 0);
  tbt_pha_ch0_valid_o                       : out std_logic;
  tbt_pha_ch1_o                             : out std_logic_vector(23 downto 0);
  tbt_pha_ch1_valid_o                       : out std_logic;
  tbt_pha_ch2_o                             : out std_logic_vector(23 downto 0);
  tbt_pha_ch2_valid_o                       : out std_logic;
  tbt_pha_ch3_o                             : out std_logic_vector(23 downto 0);
  tbt_pha_ch3_valid_o                       : out std_logic;

  fofb_decim_ch0_i_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch0_q_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch1_i_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch1_q_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch2_i_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch2_q_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch3_i_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_ch3_q_o                        : out std_logic_vector(23 downto 0);
  fofb_decim_valid_o                        : out std_logic;

  fofb_decim_q_01_missing_o                 : out std_logic;
  fofb_decim_q_23_missing_o                 : out std_logic;

  fofb_amp_ch0_o                            : out std_logic_vector(23 downto 0);
  fofb_amp_ch0_valid_o                      : out std_logic;
  fofb_amp_ch1_o                            : out std_logic_vector(23 downto 0);
  fofb_amp_ch1_valid_o                      : out std_logic;
  fofb_amp_ch2_o                            : out std_logic_vector(23 downto 0);
  fofb_amp_ch2_valid_o                      : out std_logic;
  fofb_amp_ch3_o                            : out std_logic_vector(23 downto 0);
  fofb_amp_ch3_valid_o                      : out std_logic;

  fofb_pha_ch0_o                            : out std_logic_vector(23 downto 0);
  fofb_pha_ch0_valid_o                      : out std_logic;
  fofb_pha_ch1_o                            : out std_logic_vector(23 downto 0);
  fofb_pha_ch1_valid_o                      : out std_logic;
  fofb_pha_ch2_o                            : out std_logic_vector(23 downto 0);
  fofb_pha_ch2_valid_o                      : out std_logic;
  fofb_pha_ch3_o                            : out std_logic_vector(23 downto 0);
  fofb_pha_ch3_valid_o                      : out std_logic;

  monit_amp_ch0_o                           : out std_logic_vector(23 downto 0);
  monit_amp_ch0_valid_o                     : out std_logic;
  monit_amp_ch1_o                           : out std_logic_vector(23 downto 0);
  monit_amp_ch1_valid_o                     : out std_logic;
  monit_amp_ch2_o                           : out std_logic_vector(23 downto 0);
  monit_amp_ch2_valid_o                     : out std_logic;
  monit_amp_ch3_o                           : out std_logic_vector(23 downto 0);
  monit_amp_ch3_valid_o                     : out std_logic;

  monit_cic_unexpected_o                    : out std_logic;
  monit_cfir_incorrect_o                    : out std_logic;
  monit_pfir_incorrect_o                    : out std_logic;

  x_tbt_o                                   : out std_logic_vector(25 downto 0);
  x_tbt_valid_o                             : out std_logic;
  y_tbt_o                                   : out std_logic_vector(25 downto 0);
  y_tbt_valid_o                             : out std_logic;
  q_tbt_o                                   : out std_logic_vector(25 downto 0);
  q_tbt_valid_o                             : out std_logic;
  sum_tbt_o                                 : out std_logic_vector(25 downto 0);
  sum_tbt_valid_o                           : out std_logic;

  x_fofb_o                                  : out std_logic_vector(25 downto 0);
  x_fofb_valid_o                            : out std_logic;
  y_fofb_o                                  : out std_logic_vector(25 downto 0);
  y_fofb_valid_o                            : out std_logic;
  q_fofb_o                                  : out std_logic_vector(25 downto 0);
  q_fofb_valid_o                            : out std_logic;
  sum_fofb_o                                : out std_logic_vector(25 downto 0);
  sum_fofb_valid_o                          : out std_logic;

  x_monit_o                                 : out std_logic_vector(25 downto 0);
  x_monit_valid_o                           : out std_logic;
  y_monit_o                                 : out std_logic_vector(25 downto 0);
  y_monit_valid_o                           : out std_logic;
  q_monit_o                                 : out std_logic_vector(25 downto 0);
  q_monit_valid_o                           : out std_logic;
  sum_monit_o                               : out std_logic_vector(25 downto 0);
  sum_monit_valid_o                         : out std_logic;

  x_monit_1_o                               : out std_logic_vector(25 downto 0);
  x_monit_1_valid_o                         : out std_logic;
  y_monit_1_o                               : out std_logic_vector(25 downto 0);
  y_monit_1_valid_o                         : out std_logic;
  q_monit_1_o                               : out std_logic_vector(25 downto 0);
  q_monit_1_valid_o                         : out std_logic;
  sum_monit_1_o                             : out std_logic_vector(25 downto 0);
  sum_monit_1_valid_o                       : out std_logic;

  monit_pos_1_incorrect_o                   : out std_logic;

  -----------------------------
  -- Output to RFFE board
  -----------------------------
  clk_swap_o                                : out std_logic;
  ctrl1_o                                   : out std_logic_vector(7 downto 0);
  ctrl2_o                                   : out std_logic_vector(7 downto 0);

  -----------------------------
  -- Clock drivers for various rates
  -----------------------------

  clk_ce_1_o                                : out std_logic;
  clk_ce_1112_o                             : out std_logic;
  clk_ce_11120000_o                         : out std_logic;
  clk_ce_111200000_o                        : out std_logic;
  clk_ce_1390000_o                          : out std_logic;
  clk_ce_2_o                                : out std_logic;
  clk_ce_2224_o                             : out std_logic;
  clk_ce_22240000_o                         : out std_logic;
  clk_ce_222400000_o                        : out std_logic;
  clk_ce_2780000_o                          : out std_logic;
  clk_ce_35_o                               : out std_logic;
  clk_ce_5000_o                             : out std_logic;
  clk_ce_556_o                              : out std_logic;
  clk_ce_5560000_o                          : out std_logic;
  clk_ce_70_o                               : out std_logic
);
end wb_position_calc_core;

architecture rtl of wb_position_calc_core is

  ---------------------------------------------------------
  --                     Constants                       --
  ---------------------------------------------------------

  constant c_cdc_data_ref_width             : natural := 4*c_dsp_ref_num_bits; -- c_num_adc_channels ?
  constant c_cdc_data_ref_iq_width          : natural := 8*c_dsp_ref_num_bits; -- c_num_adc_channels*2 ?
  constant c_cdc_data_pos_width             : natural := 4*c_dsp_pos_num_bits; -- c_num_adc_channels ?
  constant c_cdc_ref_size                   : natural := 16;

  constant c_num_pipeline_regs              : integer := 8;

  ---------------------------------------------------------
  --                  General Signals                    --
  ---------------------------------------------------------

  signal sys_clr                            : std_logic;

  -- Try to reduce fanout of clear signal
  attribute MAX_FANOUT: string;
  attribute MAX_FANOUT of sys_clr: signal is "REDUCE";

  ---------------------------------------------------------
  --               ADC, MIX and BPF data                 --
  ---------------------------------------------------------

  signal adc_ch0_sp                         : std_logic_vector(15 downto 0);
  signal adc_ch1_sp                         : std_logic_vector(15 downto 0);
  signal adc_ch2_sp                         : std_logic_vector(15 downto 0);
  signal adc_ch3_sp                         : std_logic_vector(15 downto 0);

  signal bpf_ch0                            : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal bpf_ch1                            : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal bpf_ch2                            : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal bpf_ch3                            : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal bpf_valid                          : std_logic := '1';

  signal mix_ch0_i                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch0_q                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch1_i                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch1_q                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch2_i                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch2_q                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch3_i                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_ch3_q                          : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal mix_valid                          : std_logic := '1';

  ---------------------------------------------------------
  --                     TBT data                        --
  ---------------------------------------------------------

  signal tbt_decim_ch0_i                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch0_q                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch1_i                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch1_q                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch2_i                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch2_q                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch3_i                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_ch3_q                    : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_decim_valid                    : std_logic := '1';

  signal tbt_amp_ch0                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_amp_ch0_valid                  : std_logic := '1';
  signal tbt_amp_ch1                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_amp_ch1_valid                  : std_logic := '1';
  signal tbt_amp_ch2                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_amp_ch2_valid                  : std_logic := '1';
  signal tbt_amp_ch3                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_amp_ch3_valid                  : std_logic := '1';

  signal tbt_pha_ch0                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_pha_ch0_valid                  : std_logic := '1';
  signal tbt_pha_ch1                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_pha_ch1_valid                  : std_logic := '1';
  signal tbt_pha_ch2                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_pha_ch2_valid                  : std_logic := '1';
  signal tbt_pha_ch3                        : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal tbt_pha_ch3_valid                  : std_logic := '1';

  ---------------------------------------------------------
  --                     FOFB data                       --
  ---------------------------------------------------------

  signal fofb_decim_ch0_i                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch0_q                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch1_i                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch1_q                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch2_i                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch2_q                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch3_i                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_ch3_q                   : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_decim_valid                   : std_logic := '1';

  signal fofb_amp_ch0                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_amp_ch0_valid                 : std_logic := '1';
  signal fofb_amp_ch1                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_amp_ch1_valid                 : std_logic := '1';
  signal fofb_amp_ch2                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_amp_ch2_valid                 : std_logic := '1';
  signal fofb_amp_ch3                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_amp_ch3_valid                 : std_logic := '1';

  signal fofb_pha_ch0                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_pha_ch0_valid                 : std_logic := '1';
  signal fofb_pha_ch1                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_pha_ch1_valid                 : std_logic := '1';
  signal fofb_pha_ch2                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_pha_ch2_valid                 : std_logic := '1';
  signal fofb_pha_ch3                       : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal fofb_pha_ch3_valid                 : std_logic := '1';

  ---------------------------------------------------------
  --                   Monitoring data                   --
  ---------------------------------------------------------

  signal monit_amp_ch0                      : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal monit_amp_ch0_valid                : std_logic := '1';
  signal monit_amp_ch1                      : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal monit_amp_ch1_valid                : std_logic := '1';
  signal monit_amp_ch2                      : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal monit_amp_ch2_valid                : std_logic := '1';
  signal monit_amp_ch3                      : std_logic_vector(c_dsp_ref_num_bits-1 downto 0);
  signal monit_amp_ch3_valid                : std_logic := '1';

  ---------------------------------------------------------
  --                   Position data                     --
  ---------------------------------------------------------

  signal x_tbt                              : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal x_tbt_valid                        : std_logic := '1';
  signal y_tbt                              : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal y_tbt_valid                        : std_logic := '1';
  signal q_tbt                              : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal q_tbt_valid                        : std_logic := '1';
  signal sum_tbt                            : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal sum_tbt_valid                      : std_logic := '1';

  signal x_fofb                             : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal x_fofb_valid                       : std_logic := '1';
  signal y_fofb                             : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal y_fofb_valid                       : std_logic := '1';
  signal q_fofb                             : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal q_fofb_valid                       : std_logic := '1';
  signal sum_fofb                           : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal sum_fofb_valid                     : std_logic := '1';

  signal x_monit                            : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal x_monit_valid                      : std_logic := '1';
  signal y_monit                            : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal y_monit_valid                      : std_logic := '1';
  signal q_monit                            : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal q_monit_valid                      : std_logic := '1';
  signal sum_monit                          : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal sum_monit_valid                    : std_logic := '1';

  signal x_monit_1                          : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal x_monit_1_valid                    : std_logic := '1';
  signal y_monit_1                          : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal y_monit_1_valid                    : std_logic := '1';
  signal q_monit_1                          : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal q_monit_1_valid                    : std_logic := '1';
  signal sum_monit_1                        : std_logic_vector(c_dsp_pos_num_bits-1 downto 0);
  signal sum_monit_1_valid                  : std_logic := '1';

  ---------------------------------------------------------
  -- FIFO CDC signals
  ---------------------------------------------------------

  signal fifo_bpf_in                        : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_bpf_out                       : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_bpf_valid_in                  : std_logic;
  signal fifo_bpf_valid_out                 : std_logic;

  signal fifo_mix_in                        : std_logic_vector(c_cdc_data_ref_iq_width-1 downto 0);
  signal fifo_mix_out                       : std_logic_vector(c_cdc_data_ref_iq_width-1 downto 0);
  signal fifo_mix_valid_in                  : std_logic;
  signal fifo_mix_valid_out                 : std_logic;

  signal fifo_tbt_decim_in                  : std_logic_vector(c_cdc_data_ref_iq_width-1 downto 0);
  signal fifo_tbt_decim_out                 : std_logic_vector(c_cdc_data_ref_iq_width-1 downto 0);
  signal fifo_tbt_decim_valid_in            : std_logic;
  signal fifo_tbt_decim_valid_out           : std_logic;

  signal fifo_tbt_amp_in                    : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_tbt_amp_out                   : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_tbt_amp_valid_in              : std_logic;
  signal fifo_tbt_amp_valid_out             : std_logic;

  signal fifo_tbt_pha_in                    : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_tbt_pha_out                   : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_tbt_pha_valid_in              : std_logic;
  signal fifo_tbt_pha_valid_out             : std_logic;

  signal fifo_tbt_pos_in                    : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_tbt_pos_out                   : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_tbt_pos_valid_in              : std_logic;
  signal fifo_tbt_pos_valid_out             : std_logic;

  signal fifo_fofb_decim_in                 : std_logic_vector(c_cdc_data_ref_iq_width-1 downto 0);
  signal fifo_fofb_decim_out                : std_logic_vector(c_cdc_data_ref_iq_width-1 downto 0);
  signal fifo_fofb_decim_valid_in           : std_logic;
  signal fifo_fofb_decim_valid_out          : std_logic;

  signal fifo_fofb_amp_in                   : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_fofb_amp_out                  : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_fofb_amp_valid_in             : std_logic;
  signal fifo_fofb_amp_valid_out            : std_logic;

  signal fifo_fofb_pha_in                   : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_fofb_pha_out                  : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_fofb_pha_valid_in             : std_logic;
  signal fifo_fofb_pha_valid_out            : std_logic;

  signal fifo_fofb_pos_in                   : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_fofb_pos_out                  : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_fofb_pos_valid_in             : std_logic;
  signal fifo_fofb_pos_valid_out            : std_logic;

  signal fifo_monit_amp_in                  : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_monit_amp_out                 : std_logic_vector(c_cdc_data_ref_width-1 downto 0);
  signal fifo_monit_amp_valid_in            : std_logic;
  signal fifo_monit_amp_valid_out           : std_logic;

  signal fifo_monit_pos_in                  : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_monit_pos_out                 : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_monit_pos_valid_in            : std_logic;
  signal fifo_monit_pos_valid_out           : std_logic;

  signal fifo_monit_1_pos_in                : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_monit_1_pos_out               : std_logic_vector(c_cdc_data_pos_width-1 downto 0);
  signal fifo_monit_1_pos_valid_in          : std_logic;
  signal fifo_monit_1_pos_valid_out         : std_logic;

  ---------------------------------------------------------
  --                Clock Enable signals                 --
  ---------------------------------------------------------
  signal clk_ce_1                            : std_logic;
  signal clk_ce_1112                         : std_logic;
  signal clk_ce_1390000                      : std_logic;
  signal clk_ce_2                            : std_logic;
  signal clk_ce_2224                         : std_logic;
  signal clk_ce_22240000                     : std_logic;
  signal clk_ce_222400000                    : std_logic;
  signal clk_ce_2780000                      : std_logic;
  signal clk_ce_35                           : std_logic;
  signal clk_ce_5000                         : std_logic;
  signal clk_ce_556                          : std_logic;
  signal clk_ce_5560000                      : std_logic;
  signal clk_ce_70                           : std_logic;

  signal clk_ce_11120000_int                 : std_logic;
  signal clk_ce_111200000_int                : std_logic;

begin
  -- fs_rst_n_i                              => fs_rst_n_i,
  sys_clr                                   <= not fs_rst_n_i;

  -- FIX ME! Wishbone interface goes directly through here!
  gen_with_switching : if (g_with_switching = 1) generate
    cmp_wb_bpm_swap : wb_bpm_swap
    generic map
    (
      g_interface_mode                          => g_interface_mode,
      g_address_granularity                     => g_address_granularity
    )
    port map
    (
      rst_n_i                                   => rst_n_i,
      clk_sys_i                                 => clk_i,
      fs_rst_n_i                                => fs_rst_n_i,
      fs_clk_i                                  => fs_clk2x_i,

      -----------------------------
      -- Wishbone signals
      -----------------------------
      wb_adr_i                                  => wb_adr_i,
      wb_dat_i                                  => wb_dat_i,
      wb_dat_o                                  => wb_dat_o,
      wb_sel_i                                  => wb_sel_i,
      wb_we_i                                   => wb_we_i,
      wb_cyc_i                                  => wb_cyc_i,
      wb_stb_i                                  => wb_stb_i,
      wb_ack_o                                  => wb_ack_o,
      wb_stall_o                                => wb_stall_o,

      -----------------------------
      -- External ports
      -----------------------------
      -- Input from ADC FMC board
      cha_i                                     => adc_ch0_i,
      chb_i                                     => adc_ch1_i,
      chc_i                                     => adc_ch2_i,
      chd_i                                     => adc_ch3_i,

      -- Output to data processing level
      cha_o                                     => adc_ch0_sp,
      chb_o                                     => adc_ch1_sp,
      chc_o                                     => adc_ch2_sp,
      chd_o                                     => adc_ch3_sp,

      -- Output to RFFE board
      clk_swap_o                                => clk_swap_o,
      ctrl1_o                                   => ctrl1_o,
      ctrl2_o                                   => ctrl2_o
    );
  end generate;

  -- Bypass switching module
  gen_without_switching : if (g_with_switching = 0) generate
    wb_dat_o <= (others => '0');
    wb_ack_o <= '0';
    wb_stall_o <= '0';
    clk_swap_o <= '0';
    ctrl1_o <= (others => '0');
    ctrl2_o <= (others => '0');

    adc_ch0_sp <= adc_ch0_i;
    adc_ch1_sp <= adc_ch1_i;
    adc_ch2_sp <= adc_ch2_i;
    adc_ch3_sp <= adc_ch3_i;
  end generate;

  cmp_position_calc: position_calc
  generic map (
    g_pipeline_regs                         => c_num_pipeline_regs
  )
  port map (
    --adc_ch0_i                               => adc_ch0_i,
    --adc_ch1_i                               => adc_ch1_i,
    --adc_ch2_i                               => adc_ch2_i,
    --adc_ch3_i                               => adc_ch3_i,

    adc_ch0_i                               => adc_ch0_sp,
    adc_ch1_i                               => adc_ch1_sp,
    adc_ch2_i                               => adc_ch2_sp,
    adc_ch3_i                               => adc_ch3_sp,

    clk                                     => fs_clk2x_i,
    clr                                     => sys_clr,

    del_sig_div_fofb_thres_i                => del_sig_div_fofb_thres_i,
    del_sig_div_monit_thres_i               => del_sig_div_monit_thres_i,
    del_sig_div_tbt_thres_i                 => del_sig_div_tbt_thres_i,

    ksum_i                                  => ksum_i,
    kx_i                                    => kx_i,
    ky_i                                    => ky_i,

    dds_config_valid_ch0_i                  => dds_config_valid_ch0_i,
    dds_config_valid_ch1_i                  => dds_config_valid_ch1_i,
    dds_config_valid_ch2_i                  => dds_config_valid_ch2_i,
    dds_config_valid_ch3_i                  => dds_config_valid_ch3_i,
    dds_pinc_ch0_i                          => dds_pinc_ch0_i,
    dds_pinc_ch1_i                          => dds_pinc_ch1_i,
    dds_pinc_ch2_i                          => dds_pinc_ch2_i,
    dds_pinc_ch3_i                          => dds_pinc_ch3_i,
    dds_poff_ch0_i                          => dds_poff_ch0_i,
    dds_poff_ch1_i                          => dds_poff_ch1_i,
    dds_poff_ch2_i                          => dds_poff_ch2_i,
    dds_poff_ch3_i                          => dds_poff_ch3_i,

    adc_ch0_dbg_data_o                      => adc_ch0_dbg_data_o,
    adc_ch1_dbg_data_o                      => adc_ch1_dbg_data_o,
    adc_ch2_dbg_data_o                      => adc_ch2_dbg_data_o,
    adc_ch3_dbg_data_o                      => adc_ch3_dbg_data_o,

    bpf_ch0_o                               => bpf_ch0,
    bpf_ch1_o                               => bpf_ch1,
    bpf_ch2_o                               => bpf_ch2,
    bpf_ch3_o                               => bpf_ch3,

    mix_ch0_i_o                             => mix_ch0_i,
    mix_ch0_q_o                             => mix_ch0_q,
    mix_ch1_i_o                             => mix_ch1_i,
    mix_ch1_q_o                             => mix_ch1_q,
    mix_ch2_i_o                             => mix_ch2_i,
    mix_ch2_q_o                             => mix_ch2_q,
    mix_ch3_i_o                             => mix_ch3_i,
    mix_ch3_q_o                             => mix_ch3_q,

    tbt_decim_ch0_i_o                       => tbt_decim_ch0_i,
    tbt_decim_ch0_q_o                       => tbt_decim_ch0_q,
    tbt_decim_ch1_i_o                       => tbt_decim_ch1_i,
    tbt_decim_ch1_q_o                       => tbt_decim_ch1_q,
    tbt_decim_ch2_i_o                       => tbt_decim_ch2_i,
    tbt_decim_ch2_q_o                       => tbt_decim_ch2_q,
    tbt_decim_ch3_i_o                       => tbt_decim_ch3_i,
    tbt_decim_ch3_q_o                       => tbt_decim_ch3_q,

    tbt_decim_q_ch01_incorrect_o            => tbt_decim_q_ch01_incorrect_o,
    tbt_decim_q_ch23_incorrect_o            => tbt_decim_q_ch23_incorrect_o,

    tbt_amp_ch0_o                           => tbt_amp_ch0,
    tbt_amp_ch1_o                           => tbt_amp_ch1,
    tbt_amp_ch2_o                           => tbt_amp_ch2,
    tbt_amp_ch3_o                           => tbt_amp_ch3,

    tbt_pha_ch0_o                           => tbt_pha_ch0,
    tbt_pha_ch1_o                           => tbt_pha_ch1,
    tbt_pha_ch2_o                           => tbt_pha_ch2,
    tbt_pha_ch3_o                           => tbt_pha_ch3,

    fofb_decim_ch0_i_o                      => fofb_decim_ch0_i,
    fofb_decim_ch0_q_o                      => fofb_decim_ch0_q,
    fofb_decim_ch1_i_o                      => fofb_decim_ch1_i,
    fofb_decim_ch1_q_o                      => fofb_decim_ch1_q,
    fofb_decim_ch2_i_o                      => fofb_decim_ch2_i,
    fofb_decim_ch2_q_o                      => fofb_decim_ch2_q,
    fofb_decim_ch3_i_o                      => fofb_decim_ch3_i,
    fofb_decim_ch3_q_o                      => fofb_decim_ch3_q,

    fofb_decim_q_01_missing_o               => fofb_decim_q_01_missing_o,
    fofb_decim_q_23_missing_o               => fofb_decim_q_23_missing_o,

    fofb_amp_ch0_o                          => fofb_amp_ch0,
    fofb_amp_ch1_o                          => fofb_amp_ch1,
    fofb_amp_ch2_o                          => fofb_amp_ch2,
    fofb_amp_ch3_o                          => fofb_amp_ch3,

    fofb_pha_ch0_o                          => fofb_pha_ch0,
    fofb_pha_ch1_o                          => fofb_pha_ch1,
    fofb_pha_ch2_o                          => fofb_pha_ch2,
    fofb_pha_ch3_o                          => fofb_pha_ch3,

    monit_amp_ch0_o                         => monit_amp_ch0,
    monit_amp_ch1_o                         => monit_amp_ch1,
    monit_amp_ch2_o                         => monit_amp_ch2,
    monit_amp_ch3_o                         => monit_amp_ch3,

    monit_cic_unexpected_o                  => monit_cic_unexpected_o,
    monit_cfir_incorrect_o                  => monit_cfir_incorrect_o,
    monit_pfir_incorrect_o                  => monit_pfir_incorrect_o,

    x_tbt_o                                 => x_tbt,
    x_tbt_valid_o                           => x_tbt_valid,
    y_tbt_o                                 => y_tbt,
    y_tbt_valid_o                           => y_tbt_valid,
    q_tbt_o                                 => q_tbt,
    q_tbt_valid_o                           => q_tbt_valid,
    sum_tbt_o                               => sum_tbt,
    sum_tbt_valid_o                         => sum_tbt_valid,

    x_fofb_o                                => x_fofb,
    x_fofb_valid_o                          => x_fofb_valid,
    y_fofb_o                                => y_fofb,
    y_fofb_valid_o                          => y_fofb_valid,
    q_fofb_o                                => q_fofb,
    q_fofb_valid_o                          => q_fofb_valid,
    sum_fofb_o                              => sum_fofb,
    sum_fofb_valid_o                        => sum_fofb_valid,

    x_monit_o                               => x_monit,
    x_monit_valid_o                         => x_monit_valid,
    y_monit_o                               => y_monit,
    y_monit_valid_o                         => y_monit_valid,
    q_monit_o                               => q_monit,
    q_monit_valid_o                         => q_monit_valid,
    sum_monit_o                             => sum_monit,
    sum_monit_valid_o                       => sum_monit_valid,

    x_monit_1_o                             => x_monit_1,
    x_monit_1_valid_o                       => x_monit_1_valid,
    y_monit_1_o                             => y_monit_1,
    y_monit_1_valid_o                       => y_monit_1_valid,
    q_monit_1_o                             => q_monit_1,
    q_monit_1_valid_o                       => q_monit_1_valid,
    sum_monit_1_o                           => sum_monit_1,
    sum_monit_1_valid_o                     => sum_monit_1_valid,

    monit_pos_1_incorrect_o                 => monit_pos_1_incorrect_o,

    -- Clock drivers for various rates
    clk_ce_1_o                              => clk_ce_1,
    clk_ce_1112_o                           => clk_ce_1112,
    clk_ce_1390000_o                        => clk_ce_1390000,
    clk_ce_2_o                              => clk_ce_2,
    clk_ce_2224_o                           => clk_ce_2224,
    clk_ce_22240000_o                       => clk_ce_22240000,
    clk_ce_222400000_o                      => clk_ce_222400000,
    clk_ce_2780000_o                        => clk_ce_2780000,
    clk_ce_35_o                             => clk_ce_35,
    clk_ce_5000_o                           => clk_ce_5000,
    clk_ce_556_o                            => clk_ce_556,
    clk_ce_5560000_o                        => clk_ce_5560000,
    clk_ce_70_o                             => clk_ce_70
  );

  -- Generate missing clk_ce_11120000
  cmp_xlclockdriver_clk_ce_11120000 : xlclockdriver
    generic map (
      log_2_period => 24,
      period => 11120000,
      pipeline_regs => c_num_pipeline_regs,
      use_bufg => 0
    )
    port map (
      sysce => '1',
      sysclk => fs_clk2x_i,
      sysclr => sys_clr,
      ce => clk_ce_11120000_int,
      clk => open
    );

  clk_ce_11120000_o <= clk_ce_11120000_int;

  -- Generate missing clk_ce_111200000
  cmp_xlclockdriver_clk_ce_111200000 : xlclockdriver
    generic map (
      log_2_period => 24,
      period => 111200000,
      pipeline_regs => c_num_pipeline_regs,
      use_bufg => 0
    )
    port map (
      sysce => '1',
      sysclk => fs_clk2x_i,
      sysclr => sys_clr,
      ce => clk_ce_111200000_int,
      clk => open
    );

  clk_ce_111200000_o <= clk_ce_111200000_int;

  -- Output CE
  clk_ce_1_o         <= clk_ce_1;
  clk_ce_1112_o      <= clk_ce_1112;
  clk_ce_1390000_o   <= clk_ce_1390000;
  clk_ce_2_o         <= clk_ce_2;
  clk_ce_2224_o      <= clk_ce_2224;
  clk_ce_22240000_o  <= clk_ce_22240000;
  clk_ce_222400000_o <= clk_ce_222400000;
  clk_ce_2780000_o   <= clk_ce_2780000;
  clk_ce_35_o        <= clk_ce_35;
  clk_ce_5000_o      <= clk_ce_5000;
  clk_ce_556_o       <= clk_ce_556;
  clk_ce_5560000_o   <= clk_ce_5560000;
  clk_ce_70_o        <= clk_ce_70;

  --------------------------------------------------------------------------
  --    CDC position data (Amplitudes and Position) to fs_clk domain      --
  --------------------------------------------------------------------------

  --------------------------------------------------------------------------
  --                        MIX and BPF data                              --
  --------------------------------------------------------------------------

  -- BPF data
  cmp_position_calc_cdc_fifo_bpf : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_bpf_in,
    valid_i                                   => fifo_bpf_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_bpf_out,
    valid_o                                   => fifo_bpf_valid_out
  );

  p_reg_cdc_fifo_bpf_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_bpf_in <= (others => '0');
        fifo_bpf_valid_in <= '0';
      elsif clk_ce_2 = '1' then
        fifo_bpf_in <= bpf_ch3 & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                       bpf_ch2 & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                       bpf_ch1 & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                       bpf_ch0;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_bpf_valid_in <= bpf_valid;
      else
        fifo_bpf_valid_in <= '0';
      end if;
    end if;
  end process;

  bpf_ch3_o <= fifo_bpf_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  bpf_ch2_o <= fifo_bpf_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  bpf_ch1_o <= fifo_bpf_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  bpf_ch0_o <= fifo_bpf_out(c_dsp_ref_num_bits-1 downto 0);

  bpf_valid_o <= fifo_bpf_valid_out;

  -- MIX data
  cmp_position_calc_cdc_fifo_mix : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_iq_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_mix_in,
    valid_i                                   => fifo_mix_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_mix_out,
    valid_o                                   => fifo_mix_valid_out
  );

  p_reg_cdc_fifo_mix_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_mix_in <= (others => '0');
        fifo_mix_valid_in <= '0';
      elsif clk_ce_2 = '1' then
        fifo_mix_in <=  mix_ch3_q & -- 8*c_dsp_ref_num_bits-1 downto 7*c_dsp_ref_num_bits
                        mix_ch3_i & -- 7*c_dsp_ref_num_bits-1 downto 6*c_dsp_ref_num_bits
                        mix_ch2_q & -- 6*c_dsp_ref_num_bits-1 downto 5*c_dsp_ref_num_bits
                        mix_ch2_i & -- 5*c_dsp_ref_num_bits-1 downto 4*c_dsp_ref_num_bits
                        mix_ch1_q & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                        mix_ch1_i & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                        mix_ch0_q & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                        mix_ch0_i;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_mix_valid_in <= mix_valid;
      else
        fifo_mix_valid_in <= '0';
      end if;
    end if;
  end process;

  mix_ch3_q_o <= fifo_mix_out(8*c_dsp_ref_num_bits-1 downto 7*c_dsp_ref_num_bits);
  mix_ch3_i_o <= fifo_mix_out(7*c_dsp_ref_num_bits-1 downto 6*c_dsp_ref_num_bits);
  mix_ch2_q_o <= fifo_mix_out(6*c_dsp_ref_num_bits-1 downto 5*c_dsp_ref_num_bits);
  mix_ch2_i_o <= fifo_mix_out(5*c_dsp_ref_num_bits-1 downto 4*c_dsp_ref_num_bits);
  mix_ch1_q_o <= fifo_mix_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  mix_ch1_i_o <= fifo_mix_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  mix_ch0_q_o <= fifo_mix_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  mix_ch0_i_o <= fifo_mix_out(c_dsp_ref_num_bits-1 downto 0);

  mix_valid_o <= fifo_mix_valid_out;

  --------------------------------------------------------------------------
  --                              TBT data                                --
  --------------------------------------------------------------------------

  -- TBT Decim data
  cmp_position_calc_cdc_fifo_tbt_decim : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_iq_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_tbt_decim_in,
    valid_i                                   => fifo_tbt_decim_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_tbt_decim_out,
    valid_o                                   => fifo_tbt_decim_valid_out
  );

  p_reg_cdc_fifo_tbt_decim_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_tbt_decim_in <= (others => '0');
        fifo_tbt_decim_valid_in <= '0';
      elsif clk_ce_70 = '1' then
        fifo_tbt_decim_in <=  tbt_decim_ch3_q & -- 8*c_dsp_ref_num_bits-1 downto 7*c_dsp_ref_num_bits
                        tbt_decim_ch3_i & -- 7*c_dsp_ref_num_bits-1 downto 6*c_dsp_ref_num_bits
                        tbt_decim_ch2_q & -- 6*c_dsp_ref_num_bits-1 downto 5*c_dsp_ref_num_bits
                        tbt_decim_ch2_i & -- 5*c_dsp_ref_num_bits-1 downto 4*c_dsp_ref_num_bits
                        tbt_decim_ch1_q & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                        tbt_decim_ch1_i & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                        tbt_decim_ch0_q & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                        tbt_decim_ch0_i;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_tbt_decim_valid_in <= tbt_decim_valid;
      else
        fifo_tbt_decim_valid_in <= '0';
      end if;
    end if;
  end process;

  tbt_decim_ch3_q_o <= fifo_tbt_decim_out(8*c_dsp_ref_num_bits-1 downto 7*c_dsp_ref_num_bits);
  tbt_decim_ch3_i_o <= fifo_tbt_decim_out(7*c_dsp_ref_num_bits-1 downto 6*c_dsp_ref_num_bits);
  tbt_decim_ch2_q_o <= fifo_tbt_decim_out(6*c_dsp_ref_num_bits-1 downto 5*c_dsp_ref_num_bits);
  tbt_decim_ch2_i_o <= fifo_tbt_decim_out(5*c_dsp_ref_num_bits-1 downto 4*c_dsp_ref_num_bits);
  tbt_decim_ch1_q_o <= fifo_tbt_decim_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  tbt_decim_ch1_i_o <= fifo_tbt_decim_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  tbt_decim_ch0_q_o <= fifo_tbt_decim_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  tbt_decim_ch0_i_o <= fifo_tbt_decim_out(c_dsp_ref_num_bits-1 downto 0);

  tbt_decim_valid_o <= fifo_tbt_decim_valid_out;

  --TBT amplitudes data
  cmp_position_calc_cdc_fifo_tbt_amp : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_tbt_amp_in,
    valid_i                                   => fifo_tbt_amp_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_tbt_amp_out,
    valid_o                                   => fifo_tbt_amp_valid_out
  );

  p_reg_cdc_fifo_tbt_amp_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_tbt_amp_in <= (others => '0');
        fifo_tbt_amp_valid_in <= '0';
      elsif clk_ce_70 = '1' then
        fifo_tbt_amp_in <=  tbt_amp_ch3 & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                            tbt_amp_ch2 & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                            tbt_amp_ch1 & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                            tbt_amp_ch0;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_tbt_amp_valid_in <= tbt_amp_ch0_valid;
      else
        fifo_tbt_amp_valid_in <= '0';
      end if;
    end if;
  end process;

  tbt_amp_ch3_o <= fifo_tbt_amp_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  tbt_amp_ch2_o <= fifo_tbt_amp_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  tbt_amp_ch1_o <= fifo_tbt_amp_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  tbt_amp_ch0_o <= fifo_tbt_amp_out(c_dsp_ref_num_bits-1 downto 0);

  tbt_amp_ch3_valid_o <= fifo_tbt_amp_valid_out;
  tbt_amp_ch2_valid_o <= fifo_tbt_amp_valid_out;
  tbt_amp_ch1_valid_o <= fifo_tbt_amp_valid_out;
  tbt_amp_ch0_valid_o <= fifo_tbt_amp_valid_out;

  --TBT phase data
  cmp_position_calc_cdc_fifo_tbt_phase : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_tbt_pha_in,
    valid_i                                   => fifo_tbt_pha_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_tbt_pha_out,
    valid_o                                   => fifo_tbt_pha_valid_out
  );

  p_reg_cdc_fifo_tbt_pha_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_tbt_pha_in <= (others => '0');
        fifo_tbt_pha_valid_in <= '0';
      elsif clk_ce_70 = '1' then
        fifo_tbt_pha_in <=  tbt_pha_ch3 & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                            tbt_pha_ch2 & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                            tbt_pha_ch1 & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                            tbt_pha_ch0;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_tbt_pha_valid_in <= tbt_pha_ch0_valid;
      else
        fifo_tbt_pha_valid_in <= '0';
      end if;
    end if;
  end process;

  tbt_pha_ch3_o <= fifo_tbt_pha_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  tbt_pha_ch2_o <= fifo_tbt_pha_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  tbt_pha_ch1_o <= fifo_tbt_pha_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  tbt_pha_ch0_o <= fifo_tbt_pha_out(c_dsp_ref_num_bits-1 downto 0);

  tbt_pha_ch3_valid_o <= fifo_tbt_pha_valid_out;
  tbt_pha_ch2_valid_o <= fifo_tbt_pha_valid_out;
  tbt_pha_ch1_valid_o <= fifo_tbt_pha_valid_out;
  tbt_pha_ch0_valid_o <= fifo_tbt_pha_valid_out;

  -- TBT position data
  cmp_position_calc_cdc_fifo_tbt_pos : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_pos_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_tbt_pos_in,
    valid_i                                   => fifo_tbt_pos_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_tbt_pos_out,
    valid_o                                   => fifo_tbt_pos_valid_out
  );

  p_reg_cdc_fifo_tbt_pos_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_tbt_pos_in <= (others => '0');
        fifo_tbt_pos_valid_in <= '0';
      elsif clk_ce_70 = '1' then
        fifo_tbt_pos_in <=  sum_tbt & -- 4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits
                            q_tbt & -- 3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits
                            y_tbt & -- 2*c_dsp_pos_num_bits-1 downto c_dsp_pos_num_bits
                            x_tbt;  -- c_dsp_pos_num_bits-1 downto 0

        fifo_tbt_pos_valid_in <= x_tbt_valid;
      else
        fifo_tbt_pos_valid_in <= '0';
      end if;
    end if;
  end process;

  sum_tbt_o <= fifo_tbt_pos_out(4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits);
  q_tbt_o   <= fifo_tbt_pos_out(3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits);
  y_tbt_o   <= fifo_tbt_pos_out(2*c_dsp_pos_num_bits-1 downto c_dsp_pos_num_bits);
  x_tbt_o   <= fifo_tbt_pos_out(c_dsp_pos_num_bits-1 downto 0);

  sum_tbt_valid_o <= fifo_tbt_pos_valid_out;
  q_tbt_valid_o   <= fifo_tbt_pos_valid_out;
  y_tbt_valid_o   <= fifo_tbt_pos_valid_out;
  x_tbt_valid_o   <= fifo_tbt_pos_valid_out;

  --------------------------------------------------------------------------
  --                            FOFB data                                 --
  --------------------------------------------------------------------------

  -- FOFB Decim data
  cmp_position_calc_cdc_fifo_fofb_decim : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_iq_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_fofb_decim_in,
    valid_i                                   => fifo_fofb_decim_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_fofb_decim_out,
    valid_o                                   => fifo_fofb_decim_valid_out
  );

  p_reg_cdc_fifo_fofb_decim_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_fofb_decim_in <= (others => '0');
        fifo_fofb_decim_valid_in <= '0';
      elsif clk_ce_2224 = '1' then
        fifo_fofb_decim_in <=  fofb_decim_ch3_q & -- 8*c_dsp_ref_num_bits-1 downto 7*c_dsp_ref_num_bits
                        fofb_decim_ch3_i & -- 7*c_dsp_ref_num_bits-1 downto 6*c_dsp_ref_num_bits
                        fofb_decim_ch2_q & -- 6*c_dsp_ref_num_bits-1 downto 5*c_dsp_ref_num_bits
                        fofb_decim_ch2_i & -- 5*c_dsp_ref_num_bits-1 downto 4*c_dsp_ref_num_bits
                        fofb_decim_ch1_q & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                        fofb_decim_ch1_i & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                        fofb_decim_ch0_q & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                        fofb_decim_ch0_i;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_fofb_decim_valid_in <= fofb_decim_valid;
      else
        fifo_fofb_decim_valid_in <= '0';
      end if;
    end if;
  end process;

  fofb_decim_ch3_q_o <= fifo_fofb_decim_out(8*c_dsp_ref_num_bits-1 downto 7*c_dsp_ref_num_bits);
  fofb_decim_ch3_i_o <= fifo_fofb_decim_out(7*c_dsp_ref_num_bits-1 downto 6*c_dsp_ref_num_bits);
  fofb_decim_ch2_q_o <= fifo_fofb_decim_out(6*c_dsp_ref_num_bits-1 downto 5*c_dsp_ref_num_bits);
  fofb_decim_ch2_i_o <= fifo_fofb_decim_out(5*c_dsp_ref_num_bits-1 downto 4*c_dsp_ref_num_bits);
  fofb_decim_ch1_q_o <= fifo_fofb_decim_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  fofb_decim_ch1_i_o <= fifo_fofb_decim_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  fofb_decim_ch0_q_o <= fifo_fofb_decim_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  fofb_decim_ch0_i_o <= fifo_fofb_decim_out(c_dsp_ref_num_bits-1 downto 0);

  fofb_decim_valid_o <= fifo_fofb_decim_valid_out;

  --FOFB amplitudes data
  cmp_position_calc_cdc_fifo_fofb_amp : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_fofb_amp_in,
    valid_i                                   => fifo_fofb_amp_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_fofb_amp_out,
    valid_o                                   => fifo_fofb_amp_valid_out
  );

  p_reg_cdc_fifo_fofb_amp_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_fofb_amp_in <= (others => '0');
        fifo_fofb_amp_valid_in <= '0';
      elsif clk_ce_2224 = '1' then
        fifo_fofb_amp_in <=  fofb_amp_ch3 & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                            fofb_amp_ch2 & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                            fofb_amp_ch1 & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                            fofb_amp_ch0;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_fofb_amp_valid_in <= fofb_amp_ch0_valid;
      else
        fifo_fofb_amp_valid_in <= '0';
      end if;
    end if;
  end process;

  fofb_amp_ch3_o <= fifo_fofb_amp_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  fofb_amp_ch2_o <= fifo_fofb_amp_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  fofb_amp_ch1_o <= fifo_fofb_amp_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  fofb_amp_ch0_o <= fifo_fofb_amp_out(c_dsp_ref_num_bits-1 downto 0);

  fofb_amp_ch3_valid_o <= fifo_fofb_amp_valid_out;
  fofb_amp_ch2_valid_o <= fifo_fofb_amp_valid_out;
  fofb_amp_ch1_valid_o <= fifo_fofb_amp_valid_out;
  fofb_amp_ch0_valid_o <= fifo_fofb_amp_valid_out;

  -- FOFB phase data
  cmp_position_calc_cdc_fifo_fofb_phase : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_fofb_pha_in,
    valid_i                                   => fifo_fofb_pha_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_fofb_pha_out,
    valid_o                                   => fifo_fofb_pha_valid_out
  );

  p_reg_cdc_fifo_fofb_pha_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_fofb_pha_in <= (others => '0');
        fifo_fofb_pha_valid_in <= '0';
      elsif clk_ce_2224 = '1' then
        fifo_fofb_pha_in <=  fofb_pha_ch3 & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                            fofb_pha_ch2 & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                            fofb_pha_ch1 & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                            fofb_pha_ch0;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_fofb_pha_valid_in <= fofb_pha_ch0_valid;
      else
        fifo_fofb_pha_valid_in <= '0';
      end if;
    end if;
  end process;

  fofb_pha_ch3_o <= fifo_fofb_pha_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  fofb_pha_ch2_o <= fifo_fofb_pha_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  fofb_pha_ch1_o <= fifo_fofb_pha_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  fofb_pha_ch0_o <= fifo_fofb_pha_out(c_dsp_ref_num_bits-1 downto 0);

  fofb_pha_ch3_valid_o <= fifo_fofb_pha_valid_out;
  fofb_pha_ch2_valid_o <= fifo_fofb_pha_valid_out;
  fofb_pha_ch1_valid_o <= fifo_fofb_pha_valid_out;
  fofb_pha_ch0_valid_o <= fifo_fofb_pha_valid_out;

  -- FOFB position data
  cmp_position_calc_cdc_fifo_fofb_pos : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_pos_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_fofb_pos_in,
    valid_i                                   => fifo_fofb_pos_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_fofb_pos_out,
    valid_o                                   => fifo_fofb_pos_valid_out
  );

  p_reg_cdc_fifo_fofb_pos_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_fofb_pos_in <= (others => '0');
        fifo_fofb_pos_valid_in <= '0';
      elsif clk_ce_2224 = '1' then
        fifo_fofb_pos_in <= sum_fofb & -- 4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits
                            q_fofb &   -- 3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits
                            y_fofb &   -- 2*c_dsp_pos_num_bits-1 downto   c_dsp_pos_num_bits
                            x_fofb;    -- c_dsp_pos_num_bits-1 downto 0

        fifo_fofb_pos_valid_in <= x_fofb_valid;
      else
        fifo_fofb_pos_valid_in <= '0';
      end if;
    end if;
  end process;

  sum_fofb_o <= fifo_fofb_pos_out(4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits);
  q_fofb_o   <= fifo_fofb_pos_out(3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits);
  y_fofb_o   <= fifo_fofb_pos_out(2*c_dsp_pos_num_bits-1 downto c_dsp_pos_num_bits);
  x_fofb_o   <= fifo_fofb_pos_out(c_dsp_pos_num_bits-1 downto 0);

  sum_fofb_valid_o <= fifo_fofb_pos_valid_out;
  q_fofb_valid_o   <= fifo_fofb_pos_valid_out;
  y_fofb_valid_o   <= fifo_fofb_pos_valid_out;
  x_fofb_valid_o   <= fifo_fofb_pos_valid_out;

  --------------------------------------------------------------------------
  --                         Monitoring data                              --
  --------------------------------------------------------------------------

  -- Monitoring amplitudes data
  cmp_position_calc_cdc_fifo_monit_amp : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_ref_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_monit_amp_in,
    valid_i                                   => fifo_monit_amp_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_monit_amp_out,
    valid_o                                   => fifo_monit_amp_valid_out
  );

  p_reg_cdc_fifo_monit_amp_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_monit_amp_in <= (others => '0');
        fifo_monit_amp_valid_in <= '0';
      elsif clk_ce_22240000 = '1' then
        fifo_monit_amp_in <=  monit_amp_ch3 & -- 4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits
                            monit_amp_ch2 & -- 3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits
                            monit_amp_ch1 & -- 2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits
                            monit_amp_ch0;  -- c_dsp_ref_num_bits-1 downto 0

        fifo_monit_amp_valid_in <= monit_amp_ch0_valid;
      else
        fifo_monit_amp_valid_in <= '0';
      end if;
    end if;
  end process;

  monit_amp_ch3_o <= fifo_monit_amp_out(4*c_dsp_ref_num_bits-1 downto 3*c_dsp_ref_num_bits);
  monit_amp_ch2_o <= fifo_monit_amp_out(3*c_dsp_ref_num_bits-1 downto 2*c_dsp_ref_num_bits);
  monit_amp_ch1_o <= fifo_monit_amp_out(2*c_dsp_ref_num_bits-1 downto c_dsp_ref_num_bits);
  monit_amp_ch0_o <= fifo_monit_amp_out(c_dsp_ref_num_bits-1 downto 0);

  monit_amp_ch3_valid_o <= fifo_monit_amp_valid_out;
  monit_amp_ch2_valid_o <= fifo_monit_amp_valid_out;
  monit_amp_ch1_valid_o <= fifo_monit_amp_valid_out;
  monit_amp_ch0_valid_o <= fifo_monit_amp_valid_out;

  -- Monitoring position data
  cmp_position_calc_cdc_fifo_monit_pos : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_pos_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_monit_pos_in,
    valid_i                                   => fifo_monit_pos_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_monit_pos_out,
    valid_o                                   => fifo_monit_pos_valid_out
  );

  p_reg_cdc_fifo_monit_pos_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_monit_pos_in <= (others => '0');
        fifo_monit_pos_valid_in <= '0';
      elsif clk_ce_22240000 = '1' then
        fifo_monit_pos_in <= sum_monit & -- 4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits
                            q_monit &   -- 3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits
                            y_monit &   -- 2*c_dsp_pos_num_bits-1 downto   c_dsp_pos_num_bits
                            x_monit;    -- c_dsp_pos_num_bits-1 downto 0

        fifo_monit_pos_valid_in <= x_monit_valid;
      else
        fifo_monit_pos_valid_in <= '0';
      end if;
    end if;
  end process;

  sum_monit_o <= fifo_monit_pos_out(4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits);
  q_monit_o   <= fifo_monit_pos_out(3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits);
  y_monit_o   <= fifo_monit_pos_out(2*c_dsp_pos_num_bits-1 downto c_dsp_pos_num_bits);
  x_monit_o   <= fifo_monit_pos_out(c_dsp_pos_num_bits-1 downto 0);

  sum_monit_valid_o <= fifo_monit_pos_valid_out;
  q_monit_valid_o   <= fifo_monit_pos_valid_out;
  y_monit_valid_o   <= fifo_monit_pos_valid_out;
  x_monit_valid_o   <= fifo_monit_pos_valid_out;

  --------------------------------------------------------------------------
  --                         Monitoring 1 Hz data                         --
  --------------------------------------------------------------------------

  -- Monitoring 1 Hz position data
  cmp_position_calc_cdc_fifo_monit_1_pos : position_calc_cdc_fifo
  generic map
  (
    g_data_width                              => c_cdc_data_pos_width,
    g_size                                    => c_cdc_ref_size
  )
  port map
  (
    clk_wr_i                                  => fs_clk2x_i,
    data_i                                    => fifo_monit_1_pos_in,
    valid_i                                   => fifo_monit_1_pos_valid_in,

    clk_rd_i                                  => fs_clk_i,
    data_o                                    => fifo_monit_1_pos_out,
    valid_o                                   => fifo_monit_1_pos_valid_out
  );

  p_reg_cdc_fifo_monit_1_pos_inputs : process(fs_clk2x_i)
  begin
    if rising_edge(fs_clk2x_i) then
      if fs_rst_n_i = '0' then
        fifo_monit_1_pos_in <= (others => '0');
        fifo_monit_1_pos_valid_in <= '0';
      elsif clk_ce_222400000 = '1' then
        fifo_monit_1_pos_in <= sum_monit_1 & -- 4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits
                            q_monit_1 &   -- 3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits
                            y_monit_1 &   -- 2*c_dsp_pos_num_bits-1 downto   c_dsp_pos_num_bits
                            x_monit_1;    -- c_dsp_pos_num_bits-1 downto 0

        fifo_monit_1_pos_valid_in <= x_monit_1_valid;
      else
        fifo_monit_1_pos_valid_in <= '0';
      end if;
    end if;
  end process;

  sum_monit_1_o <= fifo_monit_1_pos_out(4*c_dsp_pos_num_bits-1 downto 3*c_dsp_pos_num_bits);
  q_monit_1_o   <= fifo_monit_1_pos_out(3*c_dsp_pos_num_bits-1 downto 2*c_dsp_pos_num_bits);
  y_monit_1_o   <= fifo_monit_1_pos_out(2*c_dsp_pos_num_bits-1 downto c_dsp_pos_num_bits);
  x_monit_1_o   <= fifo_monit_1_pos_out(c_dsp_pos_num_bits-1 downto 0);

  sum_monit_1_valid_o <= fifo_monit_1_pos_valid_out;
  q_monit_1_valid_o   <= fifo_monit_1_pos_valid_out;
  y_monit_1_valid_o   <= fifo_monit_1_pos_valid_out;
  x_monit_1_valid_o   <= fifo_monit_1_pos_valid_out;

end rtl;
