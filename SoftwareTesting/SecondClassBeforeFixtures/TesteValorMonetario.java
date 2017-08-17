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

public class TesteValorMonetario {

	@Test
	public void valorMonetarioCriacao() {
		// Fixture Setup
		// Exercise SUT
		ValorMonetario valor = new ValorMonetario(Moeda.BRL);
		// Result Verification
		assertNotNull(valor);
		// Fixture Teardown
	}
}
