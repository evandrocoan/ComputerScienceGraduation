-- A library clause declares a name as a library.  It 
-- does not create the library; it simply forward declares 
-- it. 
library ieee;
use ieee.std_logic_1164.all;

entity SomaSelecionada2 is

	port
	(
		-- Input ports
		input0	: in  std_logic_vector( 11 downto 0 );
		input1	: in  std_logic_vector( 11 downto 0 );
		input2	: in  std_logic_vector( 11 downto 0 );
		input3	: in  std_logic_vector( 11 downto 0 );
		sel		: in std_logic;

		-- Output ports
		soma	: out std_logic_vector(11 downto 0 )

	);
end entity;

architecture estrutural of SomaSelecionada2 is

	-- Declarations (optional)
	component adder_n_bits is
		generic(width: integer := 8);
		port(
			a,b: in std_logic_vector(width-1 downto 0);
			sum: out std_logic_vector(width-1 downto 0)
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
	
	signal AtoMux, BtoMux : std_logic_vector(11 downto 0);

begin

	-- Component Instantiation Statement (optional)
	SomaAB : adder_n_bits
		generic map(12)
		port map(
			a => input0,
			b => input1,
			sum => AtoMux
		);

		SomaCD : adder_n_bits
		generic map(12)
		port map(
			a => input2,
			b => input3,
			sum => BtoMux
		);
		
	mux : mux2x1nbits
		generic map(12)
		port map(
			inpt0 => AtoMux,
			inpt1 => BtoMux,
			sel => sel,
			outp => soma
		);

end architecture;