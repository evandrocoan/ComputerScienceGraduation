library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity exercicio is
	port
	(
		-- Input ports
		carregaCarga, clk, deslocar, dadoInSerial		: in  std_logic;
		dadoInParalelo									: in  std_logic_vector( 31 DOWNTO 0 );
		
		-- Output ports
		dadoOutSerial	: out std_logic;
		contagemOut		: out std_logic_vector( 4 downto 0 );
		dadoOutParalelo	: out std_logic_vector( 31 DOWNTO 0 )
	);
end entity;

architecture comportamento of exercicio is

-- A component declaration declares the interface of an entity or
-- a design unit written in another language.  VHDL requires that
-- you declare a component if you do not intend to instantiate
-- an entity directly.	The component need not declare all the
-- generics and ports in the entity.  It may omit generics/ports
-- with default values.

	component contador_nbits is

		generic
		(
			OUTPUT : NATURAL := 5
		);

		port
		(
			clk		  : in std_logic;
			reset	  : in std_logic;
			enable	  : in std_logic;
			q		  : out std_logic_vector( 4 downto 0 )
		);

	end component;

	component deslocador_nbits is

		generic
		(
			NUM_STAGES : natural := 32
		);

		port 
		(
			clk					: in std_logic;
			desloca				: in std_logic;
			carrega				: in std_logic;
			paralelo_in			: in std_logic_vector( 31 downto 0);
			paralelo_out		: out std_logic_vector( 31 downto 0);
			serial_in			: in std_logic;
			serial_out			: out std_logic
		);

	end component;

	signal dadoInterno : std_logic;

begin

	deslocador0 : deslocador_nbits
		generic map
		(
			32
		) PORT MAP 
		( 
			clk				=> clk,
			desloca			=> deslocar,
			carrega			=> 	carregaCarga,
			paralelo_in		=> dadoInParalelo,
			serial_in		=> dadoInSerial,
			serial_out		=> dadoInterno
		);
	
	deslocador1 : deslocador_nbits
		generic map
		(
			32
		)
		port map 
		(
			clk, deslocar, '0', "00000000000000000000000000000000", dadoOutParalelo, dadoInterno, dadoOutSerial
		);
	
	contador0 : contador_nbits
		generic map
		(
			5
		)
		port map 
		(
			clk,
			'0',
			dadoInterno,
			contagemOut
		);

end architecture;
