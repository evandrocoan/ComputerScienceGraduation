LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

entity clock_conv is
	port
	(
		I50MHz	: in  std_logic;
		
		-- Output ports
		O0_1Hz : out std_logic;
		O0_2Hz : out std_logic;
		O0_25Hz : out std_logic;
		O0_5Hz : out std_logic;
		O1Hz	: out std_logic;
		O2Hz	: out std_logic;
		O10Hz : out std_logic;
		O10Hza : out std_logic
	);
end clock_conv;

architecture clock_convimpl of clock_conv is

signal cnta: integer range 0 to 500000;
signal cntb: integer range 0 to 10;
signal cntc: integer range 0 to 50;
signal cntd: integer range 0 to 100;
signal cnte: integer range 0 to 200;
signal cntf: integer range 0 to 500;
signal cntg: integer range 0 to 400;
signal cnth: integer range 0 to 1000;
signal aux: STD_LOGIC;

begin

process (I50MHz)
begin
	if rising_edge(I50MHz) then
			
		if (cnta < 250000) then			
			aux <= '0';
		else	
			aux <= '1';			
		end if;	
		if (cnta = 499999) then
			cnta <= 0;
		else
			cnta <= cnta + 1;
		end if;		
		
	end if;
end process;

process (aux)
begin
	if rising_edge(aux) then
	
		if (cntb < 5) then
			O10Hz <= '0';
			O10Hza <= '0';
		else
			O10Hz <= '1';
			O10Hza <= '1';
		end if;				
		if (cntb = 9) then
			cntb <= 0;
		else
			cntb <= cntb + 1;
		end if;
		
		if (cntc < 25) then
			O2Hz <= '0';
		else
			O2Hz <= '1';
		end if;				
		if (cntc = 49) then
			cntc <= 0;
		else
			cntc <= cntc + 1;
		end if;
		
		if (cntd < 50) then
			O1Hz <= '0';
		else
			O1Hz <= '1';
		end if;				
		if (cntd = 99) then
			cntd <= 0;
		else
			cntd <= cntd + 1;
		end if;
		
		if (cnte < 100) then
			O0_5Hz <= '0';
		else
			O0_5Hz <= '1';
		end if;				
		if (cnte = 199) then
			cnte <= 0;
		else
			cnte <= cnte + 1;
		end if;
		
		if (cntf < 250) then
			O0_2Hz <= '0';
		else
			O0_2Hz <= '1';
		end if;				
		if (cntf = 499) then
			cntf <= 0;
		else
			cntf <= cntf + 1;
		end if;
		
		if (cntg < 200) then
			O0_25Hz <= '0';
		else
			O0_25Hz <= '1';
		end if;				
		if (cntg = 399) then
			cntg <= 0;
		else
			cntg <= cntg + 1;
		end if;
		
		if (cnth < 500) then
			O0_1Hz <= '0';
		else
			O0_1Hz <= '1';
		end if;				
		if (cnth = 999) then
			cnth <= 0;
		else
			cnth <= cnth + 1;
		end if;
		
	
	end if;
end process;
	
end clock_convimpl;