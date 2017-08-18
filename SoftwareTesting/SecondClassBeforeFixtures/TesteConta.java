
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.Dinheiro;
import br.ufsc.ine.leb.sistemaBancario.Transacao;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

public class TesteConta
{
    private Agencia agencia;

    @Before
    public void executedBeforeEach()
    {
        // Helper class
        this.agencia = Helper.criarAgencia();
    }

    @Test
    public void criarConta()
    {
        // Fixture Setup

        // Exercise SUT, implicit
        final Conta minhaConta = this.agencia.criarConta( "Conta Test" );

        // Result Verification
        Assert.assertNotNull( minhaConta );

        // Fixture Teardown
    }

    @Test
    public void obterTitular()
    {
        // Fixture Setup

        // Exercise SUT, implicit
        final Conta minhaConta = this.agencia.criarConta( "Conta Test" );

        // Result Verification
        Assert.assertEquals( "Conta Test", minhaConta.obterTitular() );

        // Fixture Teardown
    }

    @Test
    public void obterAgencia()
    {
        // Fixture Setup

        // Exercise SUT, implicit
        final Conta minhaConta = this.agencia.criarConta( "Conta Test" );

        // Result Verification
        Assert.assertEquals( this.agencia, minhaConta.obterAgencia() );

        // Fixture Teardown
    }

    @Test
    public void obterSaldo()
    {
        // Fixture Setup
        final ValorMonetario valorEsperado = Helper.criarValorMonetario( 0 );

        // Exercise SUT, implicit
        final Conta minhaConta = this.agencia.criarConta( "Conta Test" );

        // Result Verification
        Assert.assertEquals( valorEsperado, minhaConta.calcularSaldo() );

        // Fixture Teardown
    }

    @Test
    public void obterIdentificador()
    {
        // Fixture Setup
        final String titular = "Conta Test";
        final String valorEsperado = Helper.criarIdentificador( 1, titular );

        // Exercise SUT, implicit
        final Conta minhaConta = this.agencia.criarConta( titular );

        // Result Verification
        Assert.assertEquals( valorEsperado, minhaConta.obterIdentificador() );

        // Fixture Teardown
    }

    @Test
    public void adicionarTransacaoEntrada()
    {
        // Fixture Setup
        final int valor = 500;
        final ValorMonetario valorEsperado = Helper.criarValorMonetario( valor );

        final Conta minhaConta = this.agencia.criarConta( "Titular" );
        final Transacao transacao = Helper.criarTransacaoEntrada( valor );

        // Exercise SUT, implicit
        minhaConta.adicionarTransacao( transacao );

        // Result Verification
        Assert.assertEquals( valorEsperado, minhaConta.calcularSaldo() );

        // Fixture Teardown
    }

    @Test
    public void adicionarTransacaoSaida()
    {
        // Fixture Setup
        final int valor = 500;
        final Dinheiro valorEsperado = Helper.criarDinheiro( valor );

        final Conta minhaConta = this.agencia.criarConta( "Titular" );
        final Transacao transacao = Helper.criarTransacaoSaida( valor );

        // Exercise SUT, implicit
        minhaConta.adicionarTransacao( transacao );

        // Result Verification
        Assert.assertEquals( valorEsperado.negativo(), minhaConta.calcularSaldo().obterQuantia().negativo() );

        // Fixture Teardown
    }
}
