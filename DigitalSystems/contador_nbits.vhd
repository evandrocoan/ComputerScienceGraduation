-- Quartus II VHDL Template
-- Binary Counter

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity contador_nbits is

	generic
	(
		OUTPUT : NATURAL := 5
	);

	port
	(
		clk		  : in std_logic;
		reset	  : in std_logic;
		enable	  : in std_logic;
		q		  : out std_logic_vector( OUTPUT - 1 DOWNTO 0)
	);

end entity;

architecture rtl of contador_nbits is

signal   cnt		   : std_logic_vector( OUTPUT - 1 DOWNTO 0 );

begin

	process (clk)
	begin
		if (rising_edge(clk)) then

			if reset = '1' then
				-- Reset the counter to 0
				cnt <= (others => '0') ;
			end if;
			
			if ( unsigned(cnt) > 31 ) then
				-- Restart the counting
				cnt <= ( others => '0' );
			end if;

			if enable = '1' then
				-- Increment the counter if counting is enabled			   
				cnt <= std_logic_vector( signed( cnt ) + 1 );
			end if;
			
		end if;

		-- Output the current count
		q <= cnt;
	end process;

end rtl;
