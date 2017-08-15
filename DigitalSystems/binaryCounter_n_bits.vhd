----------------------------------------------------------------------------------
-- Company:   Federal University of Santa Catarina
-- Engineer:  Prof. Dr. Eng. Rafael Luiz Cancian
-- 
-- Create Date:    
-- Design Name: 
-- Module Name:    binaryCounter_n_bits - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
--use IEEE.std_logic_arith.ALL;


entity binaryCounter_n_bits is
	generic(width: integer := 8);
	port(
		clk, reset, enable, load: in std_logic;
		d: in std_logic_vector(width-1 downto 0);
		outp: out std_logic_vector(width-1 downto 0)
	);
end entity;

architecture Behavioral of binaryCounter_n_bits is
	signal value: std_logic_vector(width-1 downto 0);
begin
	p1: process (clk, reset)
	begin
		if (reset='1') then
			value <= (others=>'0');
		elsif (clk'event) and (clk='1') then
			if (load='1') then
				value <= d;
			elsif (enable='1') then
				value <= std_logic_vector(unsigned(value)+1);
			end if;
		end if;
	end process;
	outp <= value;
end architecture;

