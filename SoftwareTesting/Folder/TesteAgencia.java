package br.ufsc.ine.leb.sistemaBancario.testes;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;
import br.ufsc.ine.leb.sistemaBancario.Dinheiro;

public class TesteAgencia {

	private Banco banco;

	@Before
	public void executedBeforeEach() {
		SistemaBancario sistemaBancario = new SistemaBancario();
		banco = sistemaBancario.criarBanco("Caixa Economica", Moeda.BRL);
	}

	@Test
	public void criarCaixaEconomicaTrindade() throws Exception {
		// Fixture Setup
		// Exercise SUT
		Agencia caixaEconomicaTrindade = banco.criarAgencia("Trindade");
		// Result Verification
		assertEquals("001", caixaEconomicaTrindade.obterIdentificador());
		assertEquals("Trindade", caixaEconomicaTrindade.obterNome());
		// Fixture Teardown
	}
	
	@Test
	public void criarDuasAgencias() throws Exception {
		// Fixture Setup
		Agencia trindade = banco.criarAgencia( "Trindade" );
		Agencia serrinha = banco.criarAgencia( "Serrinha" );
		// Exercise SUT
		// Result Verification
		assertEquals("001", trindade.obterIdentificador());
		assertEquals("002", serrinha.obterIdentificador());
		// Fixture Teardown
	}
	
	@Test
	public void agenciaCriacao() {
		// Fixture Setup
		// Exercise SUT
		Agencia minhaAgencia = banco.criarAgencia("MinhaAgencia");
		// Result Verification
		assertNotNull(minhaAgencia);
		// Fixture Teardown
	}
}
