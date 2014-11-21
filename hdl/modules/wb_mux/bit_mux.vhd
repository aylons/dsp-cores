-------------------------------------------------------------------------------
-- Title      : Bit mux
-- Project    : 
-------------------------------------------------------------------------------
-- File       : bit_mux.vhd
-- Author     : aylons  <aylons@LNLS190>
-- Company    : 
-- Created    : 2014-11-14
-- Last update: 2014-11-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Single bit multiplexer with configurable number of inputs
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-11-14  1.0      aylons  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-------------------------------------------------------------------------------

entity bit_mux is

  generic (
    g_input_number : natural := 4;
    );

  port (
    data_i : std_logic_vector(g_input_number-1 downto 0);
    sel_i  : std_logic_vector(ceil(log2(g_input_number))-1 downto 0);
    data_o : std_logic;
    );

end entity bit_mux;

-------------------------------------------------------------------------------

architecture str of bit_mux is

begin  -- architecture str

  data_o <= data_i(integer(sel_i));

end architecture str;

-------------------------------------------------------------------------------
