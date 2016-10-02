----------------------------------------------------------------------------------
-- Company:   Federal University of Santa Catarina
-- Engineer:  Prof. Dr. Eng. Rafael Luiz Cancian
-- 
-- Create Date:    
-- Design Name: 
-- Module Name:    register_n_bits - Behavioral 
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


entity register_n_bits is
	generic(width: integer := 8);
	port( 
		clk, reset, writeReg: in std_logic;
		d: in std_logic_vector(width-1 downto 0);
		q: out std_logic_vector(width-1 downto 0)
	);
end entity;

architecture Behavioral of register_n_bits is
begin
	p1: process (clk, reset) 
	begin
		if (reset='1') then
			q <= (others => '0');
		elsif (clk'event and clk='1') then
			if (writeReg='1') then
				q <= d;
			end if;
		end if;
	end process;
end architecture;

