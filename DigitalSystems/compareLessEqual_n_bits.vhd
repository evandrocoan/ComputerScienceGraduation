----------------------------------------------------------------------------------
-- Company:   Federal University of Santa Catarina
-- Engineer:  Prof. Dr. Eng. Rafael Luiz Cancian
-- 
-- Create Date:    
-- Design Name: 
-- Module Name:    compareLessEqual_n_bits - Behavioral 
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

entity compareLessEqual_n_bits is
	generic(width: integer := 8);
	port( 
		inpt0, inpt1: in std_logic_vector(width-1 downto 0);
		greater, less, equal: buffer std_logic
	);
end entity;

architecture Behavioral of compareLessEqual_n_bits is
begin
	equal <= '1' when inpt0 = inpt1 else '0';
	less <= '1' when signed(inpt0) < signed(inpt1) else '0';
	greater <= '1' when (equal='0' and less='0') else '0';
end architecture;

