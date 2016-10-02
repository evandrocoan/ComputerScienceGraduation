LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY multiplicador IS
	generic(n: integer := 8);
	port(
		clk, Reset, inicio: in std_logic;
		entA, entB: in std_logic_vector(n-1 downto 0);
		saida: out std_logic_vector((2*n)-1 downto 0);
		pronto: out std_logic
	);
end entity;

architecture estrutura of multiplicador is

component bc_multiplicador IS
PORT (Reset, clk, inicio : IN STD_LOGIC;
      Az, Bz : IN STD_LOGIC;
      pronto : OUT STD_LOGIC;
      ini, CA, dec, CP: OUT STD_LOGIC );
END component;

component bo_multiplicador IS
	generic(n: integer := 8);
	PORT (clk : IN STD_LOGIC;
		ini, CP, CA, dec : IN STD_LOGIC;
		entA, entB : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0);
		Az, Bz : OUT STD_LOGIC;
		saida: OUT STD_LOGIC_VECTOR((2*n)-1 DOWNTO 0)
	);
END component;

signal ini, CP, CA, dec, Az, Bz: std_logic;

begin
	
	bc0: bc_multiplicador port map(Reset, clk, inicio,Az, Bz,pronto,ini, CA, dec, CP);
	bo0: bo_multiplicador generic map(n) port map(clk,ini, CP, CA, dec,entA, entB,Az, Bz,saida);

end architecture;