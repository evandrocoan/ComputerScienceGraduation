library ieee;
use ieee.std_logic_1164.all;
use work.UtilFunctions.all;

entity mux2x1nbits is 
	port (inpt0,inpt1: in std_logic_vector (3 downto 0);
			sel: in std_logic;
			outp: out std_logic_vector (3 downto 0));
end mux2x1nbits;

architecture arch_mult2x1 of mux2x1nbits is 
begin
	outp <= inpt0 when sel='0' else inpt1;
end arch_mult2x1;