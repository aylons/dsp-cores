library ieee;
use ieee.std_logic_1164.all;

library work;
-- Main Wishbone Definitions
use work.wishbone_pkg.all;

package dsp_cores_pkg is

  --------------------------------------------------------------------
  -- Components
  --------------------------------------------------------------------
  component position_calc
  --generic(
  --
  --);
  port(
    adc_ch0_i                                 : in std_logic_vector(15 downto 0);
    adc_ch1_i                                 : in std_logic_vector(15 downto 0);
    adc_ch2_i                                 : in std_logic_vector(15 downto 0);
    adc_ch3_i                                 : in std_logic_vector(15 downto 0);

    clk                                       : in std_logic; -- clock period = 4.44116091946435 ns (225.16635135135124 Mhz)
    clr                                       : in std_logic; -- clear signal

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

    adc_ch0_dbg_data_o                        : out std_logic_vector(15 downto 0);
    adc_ch1_dbg_data_o                        : out std_logic_vector(15 downto 0);
    adc_ch2_dbg_data_o                        : out std_logic_vector(15 downto 0);
    adc_ch3_dbg_data_o                        : out std_logic_vector(15 downto 0);

    bpf_ch0_o                                 : out std_logic_vector(23 downto 0);
    bpf_ch1_o                                 : out std_logic_vector(23 downto 0);
    bpf_ch2_o                                 : out std_logic_vector(23 downto 0);
    bpf_ch3_o                                 : out std_logic_vector(23 downto 0);

    mix_ch0_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch0_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch1_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch1_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch2_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch2_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch3_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch3_q_o                               : out std_logic_vector(23 downto 0);

    tbt_decim_ch0_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch0_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch1_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch1_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch2_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch2_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch3_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch3_q_o                         : out std_logic_vector(23 downto 0);

    tbt_decim_q_ch01_incorrect_o              : out std_logic;
    tbt_decim_q_ch23_incorrect_o              : out std_logic;

    tbt_amp_ch0_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch1_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch2_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch3_o                             : out std_logic_vector(23 downto 0);

    tbt_pha_ch0_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch1_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch2_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch3_o                             : out std_logic_vector(23 downto 0);

    fofb_decim_ch0_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch0_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch1_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch1_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch2_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch2_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch3_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch3_q_o                        : out std_logic_vector(23 downto 0);

    fofb_decim_q_01_missing_o                 : out std_logic;
    fofb_decim_q_23_missing_o                 : out std_logic;

    fofb_amp_ch0_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch1_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch2_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch3_o                            : out std_logic_vector(23 downto 0);

    fofb_pha_ch0_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch1_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch2_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch3_o                            : out std_logic_vector(23 downto 0);

    monit_amp_ch0_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch1_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch2_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch3_o                           : out std_logic_vector(23 downto 0);

    monit_cic_unexpected_o                    : out std_logic;
    monit_cfir_incorrect_o                    : out std_logic;
    monit_pfir_incorrect_o                    : out std_logic;

    x_tbt_o                                   : out std_logic_vector(25 downto 0);
    y_tbt_o                                   : out std_logic_vector(25 downto 0);
    q_tbt_o                                   : out std_logic_vector(25 downto 0);
    sum_tbt_o                                 : out std_logic_vector(25 downto 0);

    x_fofb_o                                  : out std_logic_vector(25 downto 0);
    y_fofb_o                                  : out std_logic_vector(25 downto 0);
    q_fofb_o                                  : out std_logic_vector(25 downto 0);
    sum_fofb_o                                : out std_logic_vector(25 downto 0);

    x_monit_o                                 : out std_logic_vector(25 downto 0);
    y_monit_o                                 : out std_logic_vector(25 downto 0);
    q_monit_o                                 : out std_logic_vector(25 downto 0);
    sum_monit_o                               : out std_logic_vector(25 downto 0);

    x_monit_1_o                               : out std_logic_vector(25 downto 0);
    y_monit_1_o                               : out std_logic_vector(25 downto 0);
    q_monit_1_o                               : out std_logic_vector(25 downto 0);
    sum_monit_1_o                             : out std_logic_vector(25 downto 0);

    monit_pos_1_incorrect_o                   : out std_logic;

    -- Clock drivers for various rates
    clk_ce_1_o                                : out std_logic;
    clk_ce_1112_o                             : out std_logic;
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
  end component;

  component ddc_bpm_476_066_cw
  port (
    adc_ch0_i: in std_logic_vector(15 downto 0);
    adc_ch1_i: in std_logic_vector(15 downto 0);
    adc_ch2_i: in std_logic_vector(15 downto 0);
    adc_ch3_i: in std_logic_vector(15 downto 0);
    ce: in std_logic := '1';
    ce_clr: in std_logic := '1';
    clk: in std_logic; -- clock period = 4.44116091946435 ns (225.16635135135124 Mhz)
    dds_config_valid_ch0_i: in std_logic;
    dds_config_valid_ch1_i: in std_logic;
    dds_config_valid_ch2_i: in std_logic;
    dds_config_valid_ch3_i: in std_logic;
    dds_pinc_ch0_i: in std_logic_vector(29 downto 0);
    dds_pinc_ch1_i: in std_logic_vector(29 downto 0);
    dds_pinc_ch2_i: in std_logic_vector(29 downto 0);
    dds_pinc_ch3_i: in std_logic_vector(29 downto 0);
    dds_poff_ch0_i: in std_logic_vector(29 downto 0);
    dds_poff_ch1_i: in std_logic_vector(29 downto 0);
    dds_poff_ch2_i: in std_logic_vector(29 downto 0);
    dds_poff_ch3_i: in std_logic_vector(29 downto 0);
    del_sig_div_fofb_thres_i: in std_logic_vector(25 downto 0);
    del_sig_div_monit_thres_i: in std_logic_vector(25 downto 0);
    del_sig_div_tbt_thres_i: in std_logic_vector(25 downto 0);
    ksum_i: in std_logic_vector(24 downto 0);
    kx_i: in std_logic_vector(24 downto 0);
    ky_i: in std_logic_vector(24 downto 0);
    adc_ch0_dbg_data_o: out std_logic_vector(15 downto 0);
    adc_ch1_dbg_data_o: out std_logic_vector(15 downto 0);
    adc_ch2_dbg_data_o: out std_logic_vector(15 downto 0);
    adc_ch3_dbg_data_o: out std_logic_vector(15 downto 0);
    bpf_ch0_o: out std_logic_vector(23 downto 0);
    bpf_ch1_o: out std_logic_vector(23 downto 0);
    bpf_ch2_o: out std_logic_vector(23 downto 0);
    bpf_ch3_o: out std_logic_vector(23 downto 0);
    cic_fofb_q_01_missing_o: out std_logic;
    cic_fofb_q_23_missing_o: out std_logic;
    fofb_amp_ch0_o: out std_logic_vector(23 downto 0);
    fofb_amp_ch1_o: out std_logic_vector(23 downto 0);
    fofb_amp_ch2_o: out std_logic_vector(23 downto 0);
    fofb_amp_ch3_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch0_i_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch0_q_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch1_i_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch1_q_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch2_i_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch2_q_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch3_i_o: out std_logic_vector(23 downto 0);
    fofb_decim_ch3_q_o: out std_logic_vector(23 downto 0);
    fofb_pha_ch0_o: out std_logic_vector(23 downto 0);
    fofb_pha_ch1_o: out std_logic_vector(23 downto 0);
    fofb_pha_ch2_o: out std_logic_vector(23 downto 0);
    fofb_pha_ch3_o: out std_logic_vector(23 downto 0);
    mix_ch0_i_o: out std_logic_vector(23 downto 0);
    mix_ch0_q_o: out std_logic_vector(23 downto 0);
    mix_ch1_i_o: out std_logic_vector(23 downto 0);
    mix_ch1_q_o: out std_logic_vector(23 downto 0);
    mix_ch2_i_o: out std_logic_vector(23 downto 0);
    mix_ch2_q_o: out std_logic_vector(23 downto 0);
    mix_ch3_i_o: out std_logic_vector(23 downto 0);
    mix_ch3_q_o: out std_logic_vector(23 downto 0);
    monit_amp_ch0_o: out std_logic_vector(23 downto 0);
    monit_amp_ch1_o: out std_logic_vector(23 downto 0);
    monit_amp_ch2_o: out std_logic_vector(23 downto 0);
    monit_amp_ch3_o: out std_logic_vector(23 downto 0);
    monit_cfir_incorrect_o: out std_logic;
    monit_cic_unexpected_o: out std_logic;
    monit_pfir_incorrect_o: out std_logic;
    monit_pos_1_incorrect_o: out std_logic;
    q_fofb_o: out std_logic_vector(25 downto 0);
    q_monit_1_o: out std_logic_vector(25 downto 0);
    q_monit_o: out std_logic_vector(25 downto 0);
    q_tbt_o: out std_logic_vector(25 downto 0);
    sum_fofb_o: out std_logic_vector(25 downto 0);
    sum_monit_1_o: out std_logic_vector(25 downto 0);
    sum_monit_o: out std_logic_vector(25 downto 0);
    sum_tbt_o: out std_logic_vector(25 downto 0);
    tbt_amp_ch0_o: out std_logic_vector(23 downto 0);
    tbt_amp_ch1_o: out std_logic_vector(23 downto 0);
    tbt_amp_ch2_o: out std_logic_vector(23 downto 0);
    tbt_amp_ch3_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch01_incorrect_o: out std_logic;
    tbt_decim_ch0_i_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch0_q_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch1_i_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch1_q_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch23_incorrect_o: out std_logic;
    tbt_decim_ch2_i_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch2_q_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch3_i_o: out std_logic_vector(23 downto 0);
    tbt_decim_ch3_q_o: out std_logic_vector(23 downto 0);
    tbt_pha_ch0_o: out std_logic_vector(23 downto 0);
    tbt_pha_ch1_o: out std_logic_vector(23 downto 0);
    tbt_pha_ch2_o: out std_logic_vector(23 downto 0);
    tbt_pha_ch3_o: out std_logic_vector(23 downto 0);
    x_fofb_o: out std_logic_vector(25 downto 0);
    x_monit_1_o: out std_logic_vector(25 downto 0);
    x_monit_o: out std_logic_vector(25 downto 0);
    x_tbt_o: out std_logic_vector(25 downto 0);
    y_fofb_o: out std_logic_vector(25 downto 0);
    y_monit_1_o: out std_logic_vector(25 downto 0);
    y_monit_o: out std_logic_vector(25 downto 0);
    y_tbt_o: out std_logic_vector(25 downto 0)
  );
  end component;

  component wb_bpm_swap
  generic
  (
    g_interface_mode                          : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity                     : t_wishbone_address_granularity := WORD
  );
  port
  (
    rst_n_i                                   : in std_logic;
    clk_sys_i                                 : in std_logic;
    fs_clk_i                                  : in std_logic;

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
    -- External ports
    -----------------------------
    -- Input from ADC FMC board
    cha_i                                     : in  std_logic_vector(15 downto 0);
    chb_i                                     : in  std_logic_vector(15 downto 0);
    chc_i                                     : in  std_logic_vector(15 downto 0);
    chd_i                                     : in  std_logic_vector(15 downto 0);

    -- Output to data processing level
    cha_o                                     : out std_logic_vector(15 downto 0);
    chb_o                                     : out std_logic_vector(15 downto 0);
    chc_o                                     : out std_logic_vector(15 downto 0);
    chd_o                                     : out std_logic_vector(15 downto 0);

    -- Output to RFFE board
    clk_swap_o                                : out std_logic;
    ctrl1_o                                   : out std_logic_vector(7 downto 0);
    ctrl2_o                                   : out std_logic_vector(7 downto 0)
  );
  end component;

  component xwb_bpm_swap
  generic
  (
    g_interface_mode                          : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity                     : t_wishbone_address_granularity := WORD
  );
  port
  (
    rst_n_i                                   : in std_logic;
    clk_sys_i                                 : in std_logic;
    fs_clk_i                                  : in std_logic;

    -----------------------------
    -- Wishbone signals
    -----------------------------

    wb_slv_i                                  : in t_wishbone_slave_in;
    wb_slv_o                                  : out t_wishbone_slave_out;

    -----------------------------
    -- External ports
    -----------------------------
    -- Input from ADC FMC board
    cha_i                                     : in  std_logic_vector(15 downto 0);
    chb_i                                     : in  std_logic_vector(15 downto 0);
    chc_i                                     : in  std_logic_vector(15 downto 0);
    chd_i                                     : in  std_logic_vector(15 downto 0);

    -- Output to data processing level
    cha_o                                     : out std_logic_vector(15 downto 0);
    chb_o                                     : out std_logic_vector(15 downto 0);
    chc_o                                     : out std_logic_vector(15 downto 0);
    chd_o                                     : out std_logic_vector(15 downto 0);

    -- Output to RFFE board
    clk_swap_o                                : out std_logic;
    ctrl1_o                                   : out std_logic_vector(7 downto 0);
    ctrl2_o                                   : out std_logic_vector(7 downto 0)
  );
  end component;

  component wb_position_calc_core
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
    fs_clk_i                                  : in std_logic; -- clock period = 4.44116091946435 ns (225.16635135135124 Mhz)

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

    mix_ch0_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch0_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch1_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch1_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch2_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch2_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch3_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch3_q_o                               : out std_logic_vector(23 downto 0);

    tbt_decim_ch0_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch0_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch1_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch1_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch2_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch2_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch3_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch3_q_o                         : out std_logic_vector(23 downto 0);

    tbt_decim_q_ch01_incorrect_o              : out std_logic;
    tbt_decim_q_ch23_incorrect_o              : out std_logic;

    tbt_amp_ch0_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch1_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch2_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch3_o                             : out std_logic_vector(23 downto 0);

    tbt_pha_ch0_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch1_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch2_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch3_o                             : out std_logic_vector(23 downto 0);

    fofb_decim_ch0_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch0_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch1_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch1_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch2_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch2_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch3_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch3_q_o                        : out std_logic_vector(23 downto 0);

    fofb_decim_q_01_missing_o                 : out std_logic;
    fofb_decim_q_23_missing_o                 : out std_logic;

    fofb_amp_ch0_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch1_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch2_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch3_o                            : out std_logic_vector(23 downto 0);

    fofb_pha_ch0_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch1_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch2_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch3_o                            : out std_logic_vector(23 downto 0);

    monit_amp_ch0_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch1_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch2_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch3_o                           : out std_logic_vector(23 downto 0);

    monit_cic_unexpected_o                    : out std_logic;
    monit_cfir_incorrect_o                    : out std_logic;
    monit_pfir_incorrect_o                    : out std_logic;

    x_tbt_o                                   : out std_logic_vector(25 downto 0);
    y_tbt_o                                   : out std_logic_vector(25 downto 0);
    q_tbt_o                                   : out std_logic_vector(25 downto 0);
    sum_tbt_o                                 : out std_logic_vector(25 downto 0);

    x_fofb_o                                  : out std_logic_vector(25 downto 0);
    y_fofb_o                                  : out std_logic_vector(25 downto 0);
    q_fofb_o                                  : out std_logic_vector(25 downto 0);
    sum_fofb_o                                : out std_logic_vector(25 downto 0);

    x_monit_o                                 : out std_logic_vector(25 downto 0);
    y_monit_o                                 : out std_logic_vector(25 downto 0);
    q_monit_o                                 : out std_logic_vector(25 downto 0);
    sum_monit_o                               : out std_logic_vector(25 downto 0);

    x_monit_1_o                               : out std_logic_vector(25 downto 0);
    y_monit_1_o                               : out std_logic_vector(25 downto 0);
    q_monit_1_o                               : out std_logic_vector(25 downto 0);
    sum_monit_1_o                             : out std_logic_vector(25 downto 0);

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
  end component;

  component xwb_position_calc_core
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
    fs_clk_i                                  : in std_logic; -- clock period = 4.44116091946435 ns (225.16635135135124 Mhz)

    -----------------------------
    -- Wishbone signals
    -----------------------------
    wb_slv_i                                  : in t_wishbone_slave_in;
    wb_slv_o                                  : out t_wishbone_slave_out;

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

    mix_ch0_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch0_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch1_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch1_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch2_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch2_q_o                               : out std_logic_vector(23 downto 0);
    mix_ch3_i_o                               : out std_logic_vector(23 downto 0);
    mix_ch3_q_o                               : out std_logic_vector(23 downto 0);

    tbt_decim_ch0_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch0_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch1_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch1_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch2_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch2_q_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch3_i_o                         : out std_logic_vector(23 downto 0);
    tbt_decim_ch3_q_o                         : out std_logic_vector(23 downto 0);

    tbt_decim_q_ch01_incorrect_o              : out std_logic;
    tbt_decim_q_ch23_incorrect_o              : out std_logic;

    tbt_amp_ch0_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch1_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch2_o                             : out std_logic_vector(23 downto 0);
    tbt_amp_ch3_o                             : out std_logic_vector(23 downto 0);

    tbt_pha_ch0_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch1_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch2_o                             : out std_logic_vector(23 downto 0);
    tbt_pha_ch3_o                             : out std_logic_vector(23 downto 0);

    fofb_decim_ch0_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch0_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch1_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch1_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch2_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch2_q_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch3_i_o                        : out std_logic_vector(23 downto 0);
    fofb_decim_ch3_q_o                        : out std_logic_vector(23 downto 0);

    fofb_decim_q_01_missing_o                 : out std_logic;
    fofb_decim_q_23_missing_o                 : out std_logic;

    fofb_amp_ch0_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch1_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch2_o                            : out std_logic_vector(23 downto 0);
    fofb_amp_ch3_o                            : out std_logic_vector(23 downto 0);

    fofb_pha_ch0_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch1_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch2_o                            : out std_logic_vector(23 downto 0);
    fofb_pha_ch3_o                            : out std_logic_vector(23 downto 0);

    monit_amp_ch0_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch1_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch2_o                           : out std_logic_vector(23 downto 0);
    monit_amp_ch3_o                           : out std_logic_vector(23 downto 0);

    monit_cic_unexpected_o                    : out std_logic;
    monit_cfir_incorrect_o                    : out std_logic;
    monit_pfir_incorrect_o                    : out std_logic;

    x_tbt_o                                   : out std_logic_vector(25 downto 0);
    y_tbt_o                                   : out std_logic_vector(25 downto 0);
    q_tbt_o                                   : out std_logic_vector(25 downto 0);
    sum_tbt_o                                 : out std_logic_vector(25 downto 0);

    x_fofb_o                                  : out std_logic_vector(25 downto 0);
    y_fofb_o                                  : out std_logic_vector(25 downto 0);
    q_fofb_o                                  : out std_logic_vector(25 downto 0);
    sum_fofb_o                                : out std_logic_vector(25 downto 0);

    x_monit_o                                 : out std_logic_vector(25 downto 0);
    y_monit_o                                 : out std_logic_vector(25 downto 0);
    q_monit_o                                 : out std_logic_vector(25 downto 0);
    sum_monit_o                               : out std_logic_vector(25 downto 0);

    x_monit_1_o                               : out std_logic_vector(25 downto 0);
    y_monit_1_o                               : out std_logic_vector(25 downto 0);
    q_monit_1_o                               : out std_logic_vector(25 downto 0);
    sum_monit_1_o                             : out std_logic_vector(25 downto 0);

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
  end component;

  component default_clock_driver
  -- start of user modification here!
  generic (
    pipeline_regs: integer := 5
  );
  -- end of user modification here!
  port (
    sysce: in std_logic;
    sysce_clr: in std_logic;
    sysclk: in std_logic;
    ce_1: out std_logic;
    ce_1112: out std_logic;
    ce_1390000: out std_logic;
    ce_2: out std_logic;
    ce_2224: out std_logic;
    ce_22240000: out std_logic;
    ce_222400000: out std_logic;
    ce_2780000: out std_logic;
    ce_35: out std_logic;
    ce_5000: out std_logic;
    ce_556: out std_logic;
    ce_5560000: out std_logic;
    ce_55600000: out std_logic;
    ce_70: out std_logic;
    ce_logic_1: out std_logic;
    ce_logic_1390000: out std_logic;
    ce_logic_2: out std_logic;
    ce_logic_2780000: out std_logic;
    ce_logic_556: out std_logic;
    ce_logic_5560000: out std_logic;
    clk_1: out std_logic;
    clk_1112: out std_logic;
    clk_1390000: out std_logic;
    clk_2: out std_logic;
    clk_2224: out std_logic;
    clk_22240000: out std_logic;
    clk_222400000: out std_logic;
    clk_2780000: out std_logic;
    clk_35: out std_logic;
    clk_5000: out std_logic;
    clk_556: out std_logic;
    clk_5560000: out std_logic;
    clk_55600000: out std_logic;
    clk_70: out std_logic
  );
  end component;

  component xlclockdriver
  generic (
    period: integer := 2;
    log_2_period: integer := 0;
    pipeline_regs: integer := 5;
    use_bufg: integer := 0
  );
  port (
    sysclk: in std_logic;
    sysclr: in std_logic;
    sysce: in std_logic;
    clk: out std_logic;
    clr: out std_logic;
    ce: out std_logic;
    ce_logic: out std_logic
  );
  end component;


end dsp_cores_pkg;
