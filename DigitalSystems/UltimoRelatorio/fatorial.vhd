library ieee;
use ieee.std_logic_1164.all;

entity fatorial is
	generic(n: integer := 8);
	port(
		clock, reset, inicio: in std_logic;
		entrada: in std_logic_vector(n-1 downto 0);
		saida: out std_logic_vector((4*n)-1 downto 0);
		pronto: out std_logic
	);
end entity;

architecture estrutura of fatorial is

component bc_fatorial is
	port(
		-- entradas externas de controle 
		clock, reset, inicio: in std_logic;
		-- entradas de controle/status do BO
		endLoop, endMult: in std_logic;
		-- saidas de controle p/BO
		loadAcc, loadEnt, loadCount, selAcc, enbCount, initMult: out std_logic;
		-- saidas externas de controle
		pronto: out std_logic
	);		
end component;

component bo_fatorial is
	generic(n: integer := 8);
	port(
		-- entradas de dados
		entrada: in std_logic_vector(n-1 downto 0);
		-- entradas de controle
		clock, reset: in std_logic;
		loadAcc, loadEnt, loadCount, selAcc, enbCount, initMult: in std_logic;
		-- saidas de controle/status
		endLoop, endMult: out std_logic;
		-- saidas de dados
		saida: out std_logic_vector((4*n)-1 downto 0)
	);		
end component;

signal loadAcc, loadEnt, loadCount, selAcc, enbCount, initMult, endLoop, endMult:  std_logic;

begin

	bc0: bc_fatorial port map(clock, reset, inicio, 
										endLoop, endMult,
										loadAcc, loadEnt, loadCount, selAcc, enbCount, initMult,
										pronto);		
	bo0: bo_fatorial generic map(n) port map(entrada, clock, reset,
													loadAcc, loadEnt, loadCount, selAcc, enbCount, initMult,
													endLoop, endMult,
													saida);	
end architecture;
		