library ieee;
use ieee.std_logic_1164.all;

entity bo_fatorial is
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
end entity;

architecture estrutura of bo_fatorial is

component multiplicador IS
	generic(n: integer := 8);
	port(
		clk, Reset, inicio: in std_logic;
		entA, entB: in std_logic_vector(n-1 downto 0);
		saida: out std_logic_vector((2*n)-1 downto 0);
		pronto: out std_logic
	);
end component;

component register_n_bits is
	generic(width: integer := 8);
	port( 
		clk, reset, writeReg: in std_logic;
		d: in std_logic_vector(width-1 downto 0);
		q: out std_logic_vector(width-1 downto 0)
	);
end component;

component binaryCounter_n_bits is
	generic(width: integer := 8);
	port(
		clk, reset, enable, load: in std_logic;
		d: in std_logic_vector(width-1 downto 0);
		outp: out std_logic_vector(width-1 downto 0)
	);
end component;

component mux2x1nbits is
	generic(width: integer := 8);
	port(
		inpt0: in std_logic_vector(width-1 downto 0);
		inpt1: in std_logic_vector(width-1 downto 0);
		sel: in std_logic;
		outp: out std_logic_vector(width-1 downto 0)
	);
end component;

component compareLessEqual_n_bits is
	generic(width: integer := 8);
	port( 
		inpt0, inpt1: in std_logic_vector(width-1 downto 0);
		greater, less, equal:  buffer std_logic
	);
end component;

signal UM, saidaAcc, saidaContExtend, saidaMux: std_logic_vector((4*n)-1 downto 0);
signal saidaMult: std_logic_vector((8*n)-1 downto 0);
signal DOIS, saidaEntr, saidaCount: std_logic_vector(n-1 downto 0);

begin
   saidaContExtend((4*n)-1 downto n) <= (others => '0');
	saidaContExtend(n-1 downto 0) <= saidaCount;
	DOIS(n-1 downto 2) <= (others => '0');
	DOIS(1 downto 0) <= "10";
	UM((4*n)-1 downto 1) <= (others => '0');
	UM(0) <= '1';
	saida <= saidaAcc;
	mult0: multiplicador generic map(4*n) port map(clock, reset, initMult, saidaContExtend, saidaAcc, saidaMult, endMult);
	entrd: register_n_bits generic map(n) port map(clock, reset, loadEnt, entrada, saidaEntr);
	accum: register_n_bits generic map(4*n) port map(clock, reset, loadAcc, saidaMux, saidaAcc);
	count: binaryCounter_n_bits generic map(n) port map(clock, reset, enbCount, loadCount, DOIS, saidaCount);
	muxAc: mux2x1nbits generic map(4*n) port map(UM, saidaMult((4*n)-1 downto 0), selAcc, saidaMux);
	comp0: compareLessEqual_n_bits generic map(n) port map(saidaCount,saidaEntr, endLoop, open, open);
end architecture;