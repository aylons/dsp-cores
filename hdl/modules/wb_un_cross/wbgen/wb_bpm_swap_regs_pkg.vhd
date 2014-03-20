---------------------------------------------------------------------------------------
-- Title          : Wishbone slave core for BPM Swap Channels Interface Registers
---------------------------------------------------------------------------------------
-- File           : wb_bpm_swap_regs_pkg.vhd
-- Author         : auto-generated by wbgen2 from wb_bpm_swap.wb
-- Created        : Thu Mar 20 17:19:25 2014
-- Standard       : VHDL'87
---------------------------------------------------------------------------------------
-- THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE wb_bpm_swap.wb
-- DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!
---------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bpm_swap_wbgen2_pkg is
  
  
  -- Input registers (user design -> WB slave)
  
  type t_bpm_swap_in_registers is record
    wdw_ctl_reserved_i                       : std_logic_vector(12 downto 0);
    end record;
  
  constant c_bpm_swap_in_registers_init_value: t_bpm_swap_in_registers := (
    wdw_ctl_reserved_i => (others => '0')
    );
    
    -- Output registers (WB slave -> user design)
    
    type t_bpm_swap_out_registers is record
      ctrl_rst_o                               : std_logic;
      ctrl_mode1_o                             : std_logic_vector(1 downto 0);
      ctrl_mode2_o                             : std_logic_vector(1 downto 0);
      ctrl_swap_div_f_o                        : std_logic_vector(15 downto 0);
      ctrl_clk_swap_en_o                       : std_logic;
      dly_1_o                                  : std_logic_vector(15 downto 0);
      dly_2_o                                  : std_logic_vector(15 downto 0);
      a_a_o                                    : std_logic_vector(15 downto 0);
      a_c_o                                    : std_logic_vector(15 downto 0);
      b_b_o                                    : std_logic_vector(15 downto 0);
      b_d_o                                    : std_logic_vector(15 downto 0);
      c_c_o                                    : std_logic_vector(15 downto 0);
      c_a_o                                    : std_logic_vector(15 downto 0);
      d_d_o                                    : std_logic_vector(15 downto 0);
      d_b_o                                    : std_logic_vector(15 downto 0);
      wdw_ctl_use_o                            : std_logic;
      wdw_ctl_swclk_ext_o                      : std_logic;
      wdw_ctl_rst_wdw_o                        : std_logic;
      wdw_ctl_dly_o                            : std_logic_vector(15 downto 0);
      end record;
    
    constant c_bpm_swap_out_registers_init_value: t_bpm_swap_out_registers := (
      ctrl_rst_o => '0',
      ctrl_mode1_o => (others => '0'),
      ctrl_mode2_o => (others => '0'),
      ctrl_swap_div_f_o => (others => '0'),
      ctrl_clk_swap_en_o => '0',
      dly_1_o => (others => '0'),
      dly_2_o => (others => '0'),
      a_a_o => (others => '0'),
      a_c_o => (others => '0'),
      b_b_o => (others => '0'),
      b_d_o => (others => '0'),
      c_c_o => (others => '0'),
      c_a_o => (others => '0'),
      d_d_o => (others => '0'),
      d_b_o => (others => '0'),
      wdw_ctl_use_o => '0',
      wdw_ctl_swclk_ext_o => '0',
      wdw_ctl_rst_wdw_o => '0',
      wdw_ctl_dly_o => (others => '0')
      );
    function "or" (left, right: t_bpm_swap_in_registers) return t_bpm_swap_in_registers;
    function f_x_to_zero (x:std_logic) return std_logic;
    function f_x_to_zero (x:std_logic_vector) return std_logic_vector;
end package;

package body bpm_swap_wbgen2_pkg is
function f_x_to_zero (x:std_logic) return std_logic is
begin
if(x = 'X' or x = 'U') then
return '0';
else
return x;
end if; 
end function;
function f_x_to_zero (x:std_logic_vector) return std_logic_vector is
variable tmp: std_logic_vector(x'length-1 downto 0);
begin
for i in 0 to x'length-1 loop
if(x(i) = 'X' or x(i) = 'U') then
tmp(i):= '0';
else
tmp(i):=x(i);
end if; 
end loop; 
return tmp;
end function;
function "or" (left, right: t_bpm_swap_in_registers) return t_bpm_swap_in_registers is
variable tmp: t_bpm_swap_in_registers;
begin
tmp.wdw_ctl_reserved_i := f_x_to_zero(left.wdw_ctl_reserved_i) or f_x_to_zero(right.wdw_ctl_reserved_i);
return tmp;
end function;
end package body;
