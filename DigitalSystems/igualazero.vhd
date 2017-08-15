LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY igualazero IS
	generic(n: integer := 8);
	PORT (a : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0);
	igual : OUT STD_LOGIC);
END igualazero;

ARCHITECTURE estrutura OF igualazero IS
signal ZERO: STD_LOGIC_VECTOR(n-1 DOWNTO 0);
BEGIN
	zero <= (others => '0');
	igual <= '1' WHEN A = ZERO ELSE '0';
END estrutura;