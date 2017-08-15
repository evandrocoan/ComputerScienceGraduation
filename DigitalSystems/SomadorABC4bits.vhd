library ieee;
use ieee.std_logic_1164.all;

entity SomadorABC4bits is
	port (entA, entB, entC: in std_logic_vector (3 downto 0);
			cin: in std_logic;
			s: out std_logic_vector (3 downto 0));
			
end entity;

architecture Somador of SomadorABC4bits is

	component Somador4bits is
			port 
			(cin: in std_logic;
				a, b: in std_logic_vector (3 downto 0);
				sab: out std_logic_vector (3 downto 0);
				sOver: out std_logic);
	end component;
	
	signal sab: std_logic_vector(3 downto 0);
	signal sOver: std_logic;
	
	component mux2x1nbits is
		port
		(inpt0, inpt1: in std_logic_vector (3 downto 0);
			sel: in std_logic;
			outp: out std_logic_vector (3 downto 0));
	end component;
	
	signal result: std_logic_vector(3 downto 0);
	signal over: std_logic;
	
begin
	somaAB: somador4bits port map (a=>entA, b=>entB, cin=>cin, sab => result, sOver => over);
	mux: mux2x1nbits port map (inpt0=> result, inpt1=> entC, sel=> over, outp => s);
end Somador;