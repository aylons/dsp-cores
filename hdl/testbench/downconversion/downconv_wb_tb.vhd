-------------------------------------------------------------------------------
-- Title      : Downconverter (wbs version) testbench
-- Project    : 
-------------------------------------------------------------------------------
-- File       : downconv_wb_tb.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2015-01-15
-- Last update: 2015-02-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Inputs simulated signal from file for 4 BPMs and outputs
-- calculated ABCD from Cordic in a table, using another file.
-------------------------------------------------------------------------------
-- Copyright (c) 2015 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2015-01-15  1.0      aylons  Created
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library std;
use std.textio.all;

library UNISIM;
use UNISIM.vcomponents.all;

use work.wb_stream_pkg.all;

entity downconv_wb_tb is
end downconv_wb_tb;

architecture test of downconv_wb_tb is
  constant c_input_freq : real := 250.0e6;
  constant clock_period : time := 1.0 sec / (2.0 * c_input_freq);

  -- Bus widths
  constant c_input_width  : natural := 16;
  constant c_mixed_width  : natural := 24;
  constant c_output_width : natural := 32;
  constant c_adr_in_width : natural := 2;
  constant c_tgd_in_width : natural := c_input_width+2;

  -- Mixer parameters
  constant c_sin_file         : string  := "./dds_sin.nif";
  constant c_cos_file         : string  := "./dds_cos.nif";
  constant c_number_of_points : natural := 6;
  constant c_mixer_levels     : natural := 7;
  constant c_dds_width        : natural := 6;


  -- CIC parameters
  constant c_cic_diff_delay  : natural := 1;
  constant c_cic_stages      : natural := 1;
  constant c_decimation_rate : natural := 1000;
  constant c_bus_width       : natural := natural(ceil(log2(real(c_decimation_rate))));

  -- Signals
  signal clock    : std_logic                                  := '0';
  signal adc_data : std_logic_vector(c_input_width-1 downto 0) := (others => '0');
  signal reset    : std_logic                                  := '1';
  signal ce       : std_logic;

  -- data signals
  signal input_snk_i : t_wbs_sink_in_array(3 downto 0);
  signal input_snk_o : t_wbs_sink_out_array(3 downto 0);
  signal cic_src_i   : t_wbs_source_in_array(3 downto 0);
  signal cic_src_o   : t_wbs_source_out_array(3 downto 0);

  component downconv_wb is
    generic (
      g_input_width      : natural;
      g_mixed_width      : natural;
      g_output_width     : natural;
      g_adr_in_width     : natural;
      g_tgd_in_width     : natural;
      g_sin_file         : string;
      g_cos_file         : string;
      g_dds_width        : natural;
      g_number_of_points : natural;
      g_mixer_levels     : natural;
      g_cic_diff_delay   : natural;
      g_cic_stages       : natural;
      g_decimation_rate  : natural;
      g_std_buffer       : natural);
    port (
      clk_i : in  std_logic;
      rst_i : in  std_logic;
      ce_i  : in  std_logic;
      snk_i : in  t_wbs_sink_in_array(3 downto 0);
      snk_o : out t_wbs_sink_out_array(3 downto 0);
      src_i : in  t_wbs_source_in_array(3 downto 0);
      src_o : out t_wbs_source_out_array(3 downto 0));
  end component downconv_wb;
  
begin

  clk_gen : process
  begin
    clock <= '0';
    wait for clock_period;
    clock <= '1';
    wait for clock_period;
  end process;

-- no ce for now
  ce <= '1';

  rst_gen : process(clock)
    variable clock_count : natural := 10;
  begin
    if rising_edge(clock) then
      if clock_count /= 0 then
        clock_count := clock_count - 1;
      else
        reset <= '0';
      end if;
    end if;
  end process;

  --gen_cyc : for j in 0 to 3 generate

  --  input_snk_i(j).stb <= not(input_snk_o(j).stall);
  --  input_snk_i(j).cyc <= not(input_snk_o(j).stall);

  --end generate;


  WaveGen : process
    file adc_file        : text open read_mode is "downconv4x.samples";
    file output_file     : text open write_mode is "downconv4x_output.samples";
    variable timein      : real;
    variable timeout     : real;
    variable cur_inline  : line;
    variable cur_outline : line;
    variable datain      : real;

    type t_adc_data is array(natural range <>) of std_logic_vector(c_input_width-1 downto 0);

    variable adc_data : t_adc_data(0 to 3);
    
  begin

    for j in 0 to 3 loop
      cic_src_i(j).stall <= '0';
    end loop;

    loop
      wait until clock = '0' and reset = '0';

      if input_snk_o(0).stall = '0' then

        if not endfile(adc_file) then
          readline(adc_file, cur_inline);

          read(cur_inline, timein);

          for j in 0 to 3 loop

            read(cur_inline, datain);

            adc_data(j) := std_logic_vector(to_signed(integer(datain*real(2**(c_input_width-1))), c_input_width));

            input_snk_i(j).dat(c_wbs_data_width-1 downto c_input_width) <= (others => '0');
            input_snk_i(j).dat(c_input_width-1 downto 0)                <= adc_data(j);


            input_snk_i(j).adr <= std_logic_vector(to_unsigned(j, c_wbs_address_width));

            input_snk_i(j).tgd <= std_logic_vector(to_unsigned(integer(timein*(10.0**9)), c_wbs_tgd_width-2)) & "00";
-- time in picoseconds, then zero to show this is not an endofile, rightmost bit is an error flag

            input_snk_i(j).stb <= '1';
            input_snk_i(j).cyc <= '1';
          end loop;

        else                            -- endfile = true

          for j in 0 to 3 loop

            input_snk_i(j).dat <= (others => '0');
            input_snk_i(j).adr <= std_logic_vector(to_unsigned(j, c_wbs_address_width));

            input_snk_i(j).tgd <= (c_wbs_tgd_width-1 downto 2 => '0') & "10";
--LSB  is an error flag, second is end of file

            input_snk_i(j).stb <= '1';
            input_snk_i(j).cyc <= '1';
            
          end loop;

        end if;  --endfile if

      else                              -- stall if

        for j in 0 to 3 loop
          input_snk_i(j).stb <= '0';
          input_snk_i(j).cyc <= '0';
        end loop;
        
      end if;  -- stall if

      --- Write output to file.
      if (cic_src_o(0).cyc and cic_src_o(0).stb) = '1' then

        if cic_src_o(0).tgd(0) = '1' then  -- check end of file flag
          assert(false) report "end of file" severity failure;
        end if;

        for j in 0 to 3 loop
          write(cur_outline, to_integer(unsigned(cic_src_o(j).adr)));

          --Write first the Magnitude
          write(cur_outline, ht);
          write(cur_outline, to_integer(signed(cic_src_o(j).tgd(c_output_width*2-1 downto c_output_width))));

          -- Now, write phase:

          --Write first the Magnitude
          write(cur_outline, ht);
          write(cur_outline, to_integer(signed(cic_src_o(j).tgd(c_output_width-1 downto 0))));

          write(cur_outline, ht);
          write(cur_outline, to_integer(signed(cic_src_o(j).dat)));

          writeline(output_file, cur_outline);
        end loop;

      end if;  -- output cyc and stb
      
    end loop;  -- main process loop

  end process WaveGen;



  uut : downconv_wb
    generic map (
      g_input_width      => c_input_width,
      g_mixed_width      => c_mixed_width,
      g_output_width     => c_output_width,
      g_adr_in_width     => c_adr_in_width,
      g_tgd_in_width     => c_tgd_in_width,
      g_sin_file         => c_sin_file,
      g_cos_file         => c_cos_file,
      g_dds_width        => c_dds_width,
      g_number_of_points => c_number_of_points,
      g_mixer_levels     => c_mixer_levels,
      g_cic_diff_delay   => c_cic_diff_delay,
      g_cic_stages       => c_cic_stages,
      g_decimation_rate  => c_decimation_rate,
      g_std_buffer       => 3)
    port map (
      clk_i => clock,
      rst_i => reset,
      ce_i  => ce,
      snk_i => input_snk_i,
      snk_o => input_snk_o,
      src_i => cic_src_i,
      src_o => cic_src_o);

end test;
