LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

entity ps2keyb is	
	port
	(
		ps2data	: in std_logic;		
	   ps2clk : in std_logic;
		Y	: out std_logic_vector(3 downto 0);
	   space : out std_logic;	
		enter : out std_logic;	
		esc : out std_logic	
	);
end ps2keyb;

architecture ps2keybimpl of ps2keyb is

	signal buf : std_logic_vector(0 to 10);

begin

	process (ps2clk)
	begin
		
		if falling_edge(ps2clk) then		
			buf(1 to 10) <= buf(0 to 9);
			buf(0) <= ps2data;		
		end if;	
		
		if buf(2 to 9) = "01011010" then
			enter <= '1';
		else
			enter <= '0';			
		end if;
		
		if buf(2 to 9) = "00101001" then
			space <= '1';
		else
			space <= '0';			
		end if;
		
		if buf(2 to 9) = "01110110" then
			esc <= '1';
		else
			esc <= '0';			
		end if;
				
	end process;
	
	with buf(2 to 9) select
	Y <=  "0000" when "01110000",
			"0000" when "01000101",
			"0001" when "01101001",
			"0001" when "00010110",
			"0010" when "01110010",
			"0010" when "00011110",
			"0011" when "01111010",
			"0011" when "00100110",
			"0100" when "01101011",
			"0100" when "00100101",
			"0101" when "01110011",
			"0101" when "00101110",
			"0110" when "01110100",
			"0110" when "00110110",
			"0111" when "01101100",
			"0111" when "00111101",
			"1000" when "01110101",
			"1000" when "00111110",
			"1001" when "01111101",
			"1001" when "01000110",
			"1111" when others;	
			  
end ps2keybimpl;