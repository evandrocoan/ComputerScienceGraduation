library ieee;
use ieee.std_logic_1164.all;

entity bc_fatorial is
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
end entity;

architecture FSM of bc_fatorial is
	type state_type IS (RST, INIT, CHECK, INIMULT, WAITMULT, UPDATE);
	signal estadoAtual, proxEstado: state_type;
begin
	logProxEst: process(inicio, endLoop, endMult, estadoAtual)
	begin
		case estadoAtual is
			when RST =>
				if inicio='1' then
					proxEstado <= INIT;
				else 
					proxEstado <= RST;
				end if;
			when INIT =>
				proxEstado <= CHECK;
			when CHECK =>
				if endLoop='1' then
					proxEstado <= RST;
				else
					proxEstado <= INIMULT;
				end if;
			when INIMULT =>
				proxEstado <= WAITMULT;
			when WAITMULT =>
				if endMult='1' then
					proxEstado <= UPDATE;
				else
					proxEstado <= WAITMULT;
				end if;
			when UPDATE =>
				proxEstado <= CHECK;
		end case;
	end process;

	memoria: process(clock, reset)
	begin
		if reset='1' then
			estadoAtual <= RST;
		elsif clock'event and clock='1' then
			estadoAtual <= proxEstado;
		end if;
	end process;
	
	logSaida: process(estadoAtual)
	begin
		case estadoAtual is
			when RST =>
				loadAcc <= '0';
				loadEnt <= '0';
				loadCount <= '0';
				selAcc <= '0';
				enbCount <= '0';
				initMult <= '0';
				pronto <= '1';
			when INIT =>
				loadAcc <= '1';
				loadEnt <= '1';
				loadCount <= '1';
				selAcc <= '0';
				enbCount <= '0';
				initMult <= '0';
				pronto <= '0';
			when CHECK =>
				loadAcc <= '0';
				loadEnt <= '0';
				loadCount <= '0';
				selAcc <= '0';
				enbCount <= '0';
				initMult <= '0';
				pronto <= '0';
			when INIMULT =>
				loadAcc <= '0';
				loadEnt <= '0';
				loadCount <= '0';
				selAcc <= '0';
				enbCount <= '0';
				initMult <= '1';
				pronto <= '0';
			when WAITMULT =>
				loadAcc <= '0';
				loadEnt <= '0';
				loadCount <= '0';
				selAcc <= '0';
				enbCount <= '0';
				initMult <= '0';
				pronto <= '0';
			when UPDATE =>
				loadAcc <= '1';
				loadEnt <= '0';
				loadCount <= '0';
				selAcc <= '1';
				enbCount <= '1';
				initMult <= '0';
				pronto <= '0';
		end case;
	end process;
	
end architecture;