library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.dsp_cores_pkg.all;

entity position_calc is
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
end position_calc;

architecture rtl of position_calc is

  signal ce                                  : std_logic;
  signal ce_clr                              : std_logic;

  signal clk_ce_1                            : std_logic;
  signal clk_ce_1112                         : std_logic;
  signal clk_ce_1390000                      : std_logic;
  signal clk_ce_2                            : std_logic;
  signal clk_ce_2224                         : std_logic;
  signal clk_ce_22240000                     : std_logic;
  signal clk_ce_2780000                      : std_logic;
  signal clk_ce_35                           : std_logic;
  signal clk_ce_5000                         : std_logic;
  signal clk_ce_556                          : std_logic;
  signal clk_ce_5560000                      : std_logic;
  signal clk_ce_70                           : std_logic;

begin

  ce                                        <= '1';
  --ce_clr                                    <= '0';
  ce_clr                                    <= clr;

  cmp_default_clock_driver : default_clock_driver
  generic map(
    pipeline_regs                           => 8
  )
  port map(
    sysce                                   => ce,
    sysce_clr                               => ce_clr,
    sysclk                                  => clk,
    ce_1                                    => clk_ce_1_o,
    ce_1112                                 => clk_ce_1112_o,
    ce_1390000                              => clk_ce_1390000_o,
    ce_2                                    => clk_ce_2_o,
    ce_2224                                 => clk_ce_2224_o,
    ce_22240000                             => clk_ce_22240000_o,
    ce_222400000                            => clk_ce_222400000_o,
    ce_2780000                              => clk_ce_2780000_o,
    ce_35                                   => clk_ce_35_o,
    ce_5000                                 => clk_ce_5000_o,
    ce_556                                  => clk_ce_556_o,
    ce_5560000                              => clk_ce_5560000_o,
    ce_70                                   => clk_ce_70_o,
    ce_logic_1                              => open,
    ce_logic_1390000                        => open,
    ce_logic_2                              => open,
    ce_logic_2780000                        => open,
    ce_logic_556                            => open,
    clk_1                                   => open,
    clk_1112                                => open,
    clk_1390000                             => open,
    clk_2                                   => open,
    clk_2224                                => open,
    clk_22240000                            => open,
    clk_2780000                             => open,
    clk_35                                  => open,
    clk_5000                                => open,
    clk_556                                 => open,
    clk_5560000                             => open,
    clk_70                                  => open
  );

  cmp_ddc_bpm_476_066_cw : ddc_bpm_476_066_cw
  port map (
    adc_ch0_i                               => adc_ch0_i,
    adc_ch1_i                               => adc_ch1_i,
    adc_ch2_i                               => adc_ch2_i,
    adc_ch3_i                               => adc_ch3_i,

    ce                                      => ce,
    ce_clr                                  => ce_clr,
    clk                                     => clk,

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

    bpf_ch0_o                               => bpf_ch0_o,
    bpf_ch1_o                               => bpf_ch1_o,
    bpf_ch2_o                               => bpf_ch2_o,
    bpf_ch3_o                               => bpf_ch3_o,

    mix_ch0_i_o                             => mix_ch0_i_o,
    mix_ch0_q_o                             => mix_ch0_q_o,
    mix_ch1_i_o                             => mix_ch1_i_o,
    mix_ch1_q_o                             => mix_ch1_q_o,
    mix_ch2_i_o                             => mix_ch2_i_o,
    mix_ch2_q_o                             => mix_ch2_q_o,
    mix_ch3_i_o                             => mix_ch3_i_o,
    mix_ch3_q_o                             => mix_ch3_q_o,

    tbt_decim_ch0_i_o                       => tbt_decim_ch0_i_o,
    tbt_decim_ch0_q_o                       => tbt_decim_ch0_q_o,
    tbt_decim_ch1_i_o                       => tbt_decim_ch1_i_o,
    tbt_decim_ch1_q_o                       => tbt_decim_ch1_q_o,
    tbt_decim_ch2_i_o                       => tbt_decim_ch2_i_o,
    tbt_decim_ch2_q_o                       => tbt_decim_ch2_q_o,
    tbt_decim_ch3_i_o                       => tbt_decim_ch3_i_o,
    tbt_decim_ch3_q_o                       => tbt_decim_ch3_q_o,

    tbt_decim_ch01_incorrect_o              => tbt_decim_q_ch01_incorrect_o,
    tbt_decim_ch23_incorrect_o              => tbt_decim_q_ch23_incorrect_o,

    tbt_amp_ch0_o                           => tbt_amp_ch0_o,
    tbt_amp_ch1_o                           => tbt_amp_ch1_o,
    tbt_amp_ch2_o                           => tbt_amp_ch2_o,
    tbt_amp_ch3_o                           => tbt_amp_ch3_o,

    tbt_pha_ch0_o                           => tbt_pha_ch0_o,
    tbt_pha_ch1_o                           => tbt_pha_ch1_o,
    tbt_pha_ch2_o                           => tbt_pha_ch2_o,
    tbt_pha_ch3_o                           => tbt_pha_ch3_o,

    fofb_decim_ch0_i_o                      => fofb_decim_ch0_i_o,
    fofb_decim_ch0_q_o                      => fofb_decim_ch0_q_o,
    fofb_decim_ch1_i_o                      => fofb_decim_ch1_i_o,
    fofb_decim_ch1_q_o                      => fofb_decim_ch1_q_o,
    fofb_decim_ch2_i_o                      => fofb_decim_ch2_i_o,
    fofb_decim_ch2_q_o                      => fofb_decim_ch2_q_o,
    fofb_decim_ch3_i_o                      => fofb_decim_ch3_i_o,
    fofb_decim_ch3_q_o                      => fofb_decim_ch3_q_o,

    cic_fofb_q_01_missing_o                 => fofb_decim_q_01_missing_o,
    cic_fofb_q_23_missing_o                 => fofb_decim_q_23_missing_o,

    fofb_amp_ch0_o                          => fofb_amp_ch0_o,
    fofb_amp_ch1_o                          => fofb_amp_ch1_o,
    fofb_amp_ch2_o                          => fofb_amp_ch2_o,
    fofb_amp_ch3_o                          => fofb_amp_ch3_o,

    fofb_pha_ch0_o                          => fofb_pha_ch0_o,
    fofb_pha_ch1_o                          => fofb_pha_ch1_o,
    fofb_pha_ch2_o                          => fofb_pha_ch2_o,
    fofb_pha_ch3_o                          => fofb_pha_ch3_o,

    monit_amp_ch0_o                         => monit_amp_ch0_o,
    monit_amp_ch1_o                         => monit_amp_ch1_o,
    monit_amp_ch2_o                         => monit_amp_ch2_o,
    monit_amp_ch3_o                         => monit_amp_ch3_o,

    monit_cic_unexpected_o                  => monit_cic_unexpected_o,
    monit_cfir_incorrect_o                  => monit_cfir_incorrect_o,
    monit_pfir_incorrect_o                  => monit_pfir_incorrect_o,

    x_tbt_o                                 => x_tbt_o,
    y_tbt_o                                 => y_tbt_o,
    q_tbt_o                                 => q_tbt_o,
    sum_tbt_o                               => sum_tbt_o,

    x_fofb_o                                => x_fofb_o,
    y_fofb_o                                => y_fofb_o,
    q_fofb_o                                => q_fofb_o,
    sum_fofb_o                              => sum_fofb_o,

    x_monit_o                               => x_monit_o,
    y_monit_o                               => y_monit_o,
    q_monit_o                               => q_monit_o,
    sum_monit_o                             => sum_monit_o,

    x_monit_1_o                             => x_monit_1_o,
    y_monit_1_o                             => y_monit_1_o,
    q_monit_1_o                             => q_monit_1_o,
    sum_monit_1_o                           => sum_monit_1_o,
    monit_pos_1_incorrect_o                 => monit_pos_1_incorrect_o

  );

end rtl;
