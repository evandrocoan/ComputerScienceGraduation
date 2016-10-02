LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY bo_multiplicador IS
	generic(n: integer := 8);
	PORT (clk : IN STD_LOGIC;
		ini, CP, CA, dec : IN STD_LOGIC;
		entA, entB : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0);
		Az, Bz : OUT STD_LOGIC;
		saida: OUT STD_LOGIC_VECTOR((2*n)-1 DOWNTO 0)
	);
END bo_multiplicador;

-- Sinais de comando
-- ini = RstP = mA = CB  => ini=1 somente em S1
-- CA=1 em S1 e em S4
-- dec = op = m1 = m2  => dec=1 somente em S4 (estado no qual ocorre A <= A - 1 )
-- CP=1 somente em S3 (estado no qual ocorre P <= P + B )

-- OBS: as saidas conteudoA e conteudoB so servem para inspecionar o conteudos de regA e regB
-- Assim que o codigo VHDL estiver depurado, elas deveriam ser retiradas para nao utilizar recursos

ARCHITECTURE estrutura OF bo_multiplicador IS
	
COMPONENT register_n_bits is
	generic(width: integer := 8);
	port( 
		clk, reset, writeReg: in std_logic;
		d: in std_logic_vector(width-1 downto 0);
		q: out std_logic_vector(width-1 downto 0)
	);
end COMPONENT;
	
COMPONENT mux2x1nbits is
	generic(width: integer := 8);
	port(
		inpt0: in std_logic_vector(width-1 downto 0);
		inpt1: in std_logic_vector(width-1 downto 0);
		sel: in std_logic;
		outp: out std_logic_vector(width-1 downto 0)
	);
end COMPONENT ;
	
COMPONENT somadorsubtrator IS
		generic(n: integer := 8);
		PORT (a, b : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0);
			op: IN STD_LOGIC;
			s : OUT STD_LOGIC_VECTOR(n-1 DOWNTO 0)
		);
END COMPONENT;
	
COMPONENT igualazero IS
		generic(n: integer := 8);
		PORT (a : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0);
			igual : OUT STD_LOGIC
		);
END COMPONENT;
			
	SIGNAL saimux1, saisomasubLow, saimux3, sairegA, sairegB: STD_LOGIC_VECTOR (n-1 DOWNTO 0);
	SIGNAL sairegP, saisomasub, saimux2: STD_LOGIC_VECTOR ((2*n)-1 DOWNTO 0);
	SIGNAL ZERO,UM: std_logic_vector(n-1 downto 0);

BEGIN
	ZERO <= (others => '0');
	UM(n-1 downto 1) <= (others => '0');
	UM(0) <= '1';
	saisomasubLow <= saisomasub(n-1 downto 0);
	saida <= sairegP;
	regP: register_n_bits generic map(2*n) port map (clk, ini, CP, saisomasub, sairegP);
	regA: register_n_bits generic map(n) port map (clk, '0', CA, saimux1, sairegA );
	regB: register_n_bits generic map(n) port map (clk, '0', ini, entB, sairegB );
	mux1: mux2x1nbits generic map(n) port map ( saisomasubLow, entA, ini, saimux1 );
	mux2: mux2x1nbits generic map(2*n) port map ( sairegP, ZERO&sairegA, dec, saimux2 );	
	mux3: mux2x1nbits generic map(n) port map ( sairegB, UM, dec, saimux3 );
	geraAz: igualazero generic map(n) port map ( sairegA, Az );
	geraBz: igualazero generic map(n) port map ( sairegB, Bz );	
	somasub: somadorsubtrator generic map(2*n) port map (saimux2, ZERO&saimux3, dec, saisomasub);
END estrutura;