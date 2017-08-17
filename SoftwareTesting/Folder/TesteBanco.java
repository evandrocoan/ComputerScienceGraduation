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

public class TesteBanco {

	SistemaBancario sistema;

	@Before
	public void executedBeforeEach() {
		sistema = new SistemaBancario();
	}

	private Banco criarBanco() {
		SistemaBancario sistemaBancario = new SistemaBancario();
		Banco caixaEconomica = sistemaBancario.criarBanco("Caixa Economica", Moeda.BRL);
		return caixaEconomica;
	}

	@Test
	public void bancoCriacao() {
		// Fixture Setup
		// Exercise SUT
		Banco meuBanco = criarBanco();
		// Result Verification
		assertNotNull(meuBanco);
		// Fixture Teardown
	}
}
