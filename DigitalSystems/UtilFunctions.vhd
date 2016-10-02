----------------------------------------------------------------------------------
-- Company:   Federal University of Santa Catarina
-- Engineer:  Prof. Dr. Eng. Rafael Luiz Cancian
-- 
-- Create Date:    
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 


library IEEE;
use IEEE.STD_LOGIC_1164.all;

package UtilFunctions is
	type tpMatrix is array(integer range<>,integer range<>) of std_logic;
	function max(inp1: integer; inp2: integer) return integer;
	function log2(inp : integer) return integer;
	function pow2(inp : integer) return integer;
end UtilFunctions;


package body UtilFunctions is

   function max(inp1: integer; inp2: integer) return integer is
	begin
		if (inp1 < inp2) then 
			return inp2;
		else
			return inp1;
		end if;
	end max;

	function log2(inp : integer) return integer is
	begin
		if (inp < 2) then return 0; end if;
		if (inp < 4) then return 1; end if;
		if (inp < 8) then return 2; end if;
		if (inp < 16) then return 3; end if;
		if (inp < 32) then return 4; end if;
		if (inp < 64) then return 5; end if;
		if (inp < 128) then return 6; end if;
		if (inp < 256) then return 7; end if;
		if (inp < 512) then return 8; end if;
		if (inp < 1024) then return 9; end if;
		if (inp < 2048) then return 10; end if;
		if (inp < 4096) then return 11; end if;
		if (inp < 8192) then return 12; end if;
		if (inp < 16384) then return 13; end if;
		if (inp < 32768) then return 14; end if;
		if (inp < 65536) then return 15; end if;
		return 16;
	end log2;
	
	function pow2(inp : integer) return integer is
	begin
		if (inp = 0) then return 1; end if;
		if (inp = 1) then return 2; end if;
		if (inp = 2) then return 4; end if;
		if (inp = 3) then return 8; end if;
		if (inp = 4) then return 16; end if;
		if (inp = 5) then return 32; end if;
		if (inp = 6) then return 64; end if;
		if (inp = 7) then return 128; end if;
		if (inp = 8) then return 256; end if;
		if (inp = 9) then return 512; end if;
		if (inp = 10) then return 1024; end if;
		if (inp = 11) then return 2048; end if;
		if (inp = 12) then return 4096; end if;
		if (inp = 13) then return 8192; end if;
		if (inp = 14) then return 16384; end if;
		if (inp = 15) then return 32768; end if;
		if (inp = 16) then return 65536; end if;
		return 0;
	end pow2;	

end UtilFunctions;
