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

public class TesteConta {

	private Agencia agencia;

	@Before
	public void executedBeforeEach() {
		SistemaBancario sistemaBancario = new SistemaBancario();
		Banco caixaEconomica = sistemaBancario.criarBanco("Caixa Economica", Moeda.BRL);
		agencia = caixaEconomica.criarAgencia( "Agencia" );
	}

	@Test
	public void criarConta() {
		// Fixture Setup
		// Exercise SUT
		Conta minhaConta = agencia.criarConta("Conta Test");
		// Result Verification
		assertNotNull(minhaConta);
		// Fixture Teardown
	}
}
