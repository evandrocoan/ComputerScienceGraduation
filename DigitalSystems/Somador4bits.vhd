library ieee;
use ieee.std_logic_1164.all;

entity Somador4bits is
	port (cin: in std_logic;
				a, b: in std_logic_vector (3 downto 0);
				sOver: out std_logic;
				sab: out std_logic_vector (3 downto 0));
end Somador4bits;

architecture somador of Somador4bits is
begin
	process(a,b,cin)
	variable soma: std_logic_vector (3 downto 0);
	variable c: std_logic;
	begin
		c:= cin;
		for i in 0 to 3 loop
			soma(i):=a(i) xor b(i) xor c;
			c:= (a(i) and b(i)) or ((a(i) xor b(i)) and c);
		end loop;
		sOver<=c;
		sab<= soma;
	end process;
end somador;