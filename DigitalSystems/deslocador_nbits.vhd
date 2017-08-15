-- Quartus II VHDL Template
-- Barrel Shifter

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity deslocador_nbits is

	generic
	(
		NUM_STAGES : natural := 32
	);

	port 
	(
		clk					: in std_logic;
		desloca				: in std_logic;
		carrega				: in std_logic;
		paralelo_in	: in std_logic_vector((NUM_STAGES-1) downto 0);
		paralelo_out	: out std_logic_vector((NUM_STAGES-1) downto 0);
		serial_in				: in std_logic;
		serial_out				: out std_logic
	);

end entity;

architecture rtl of deslocador_nbits is

	type byte_t is array( NUM_STAGES - 1 downto 0) of std_logic;
	signal paralelo_temp : byte_t;

begin

	process (clk)
	
	begin
	
		if ( rising_edge(clk) ) then
		
			if (desloca = '1') then
				paralelo_temp( ( NUM_STAGES - 2 ) DOWNTO 0 ) <= paralelo_temp( ( NUM_STAGES - 1 ) downto 1 );
				serial_out <= std_logic( paralelo_temp( 0 ) );
				paralelo_temp( NUM_STAGES - 1 ) <= serial_in ;
			end if;
			
			if ( carrega = '1' ) then
				paralelo_out <= std_logic_vector ( paralelo_temp );
				paralelo_temp <=  byte_t( paralelo_in );
			end if;
			
		end if;
		
	end process;

end rtl;
