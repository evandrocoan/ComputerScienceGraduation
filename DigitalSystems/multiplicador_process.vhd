library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multiplicador_process is
	generic (
		n : integer := 8
	);
	port (
		-- Entrada
		A, B                 : in std_logic_vector(n-1 downto 0);
		inicio, clock, reset : in std_logic;
		
		result : out std_logic_vector(2*n-1 downto 0);
		pronto : out std_logic
	); 
end entity;

architecture multiplicador_comportamento of multiplicador_process is 
	type tpEstado is (espera_inicio, compara_a_b, inverte_reg_b, inicio_while, multiplica, terminou_multiplicacao);
	signal estadoAtual, proximoEstado : tpEstado;
	signal regA, regB, temp : signed(n-1 downto 0);
	signal mult : signed (2*n-1 downto 0);
begin
	logicaProximoEstado: process (inicio, a, b, estadoAtual)
	begin
		case estadoAtual is
			when espera_inicio =>
				if (inicio = '0') then
					proximoEstado <= espera_inicio;
				else
					proximoEstado <= compara_a_b;
				end if;
				
			when compara_a_b =>
				if (regA > regB) then
					proximoEstado <= inverte_reg_b;
				else
					proximoEstado <= inicio_while;
				end if;
				
			when inverte_reg_b =>
				proximoEstado <= inicio_while;	
				
			when inicio_while =>
				if (regA /= 0) then
					proximoEstado <= multiplica;
				else
					proximoEstado <= terminou_multiplicacao;
				end if;
				
			when multiplica =>
				proximoEstado <= inicio_while;
				
			when terminou_multiplicacao =>
				proximoEstado <= espera_inicio;
				
		end case;
	end process;
	
	elementoMemoria: process (clock, reset)
	begin
		if reset = '1' then
			estadoAtual <= espera_inicio;
		elsif clock'event and clock='1' then
			estadoAtual <= proximoEstado;
		end if;
	end process;
	
	logicaSaida: process (estadoAtual)
	begin
		pronto <= '0';
		result <= (others => '0');
				
		case estadoAtual is
			when espera_inicio =>
				regA   <= signed(A);
				regB   <= signed(B);
				mult   <= to_signed(0, 2*n);
				
			when compara_a_b =>
			
			when inverte_reg_b =>
				temp <= regA;
				regA <= regB;
				regB <= temp;
				
			when inicio_while =>
			
			when multiplica =>
				mult <= mult + regB;
				regA <= regA - 1;
			
			when terminou_multiplicacao =>
				pronto <= '1';
				result <= std_logic_vector(mult);
		end case;
	end process;
	
end architecture;