
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;

public class TesteBanco
{
    SistemaBancario sistema;

    @Before
    public void executedBeforeEach()
    {
        this.sistema = new SistemaBancario();
    }

    private Banco criarBanco()
    {
        return this.sistema.criarBanco( "Caixa Economica", Moeda.BRL );
    }

    @Test
    public void bancoCriacao()
    {
        // Fixture Setup

        // Exercise SUT, delegated
        final Banco meuBanco = this.criarBanco();

        // Result Verification
        Assert.assertNotNull( meuBanco );

        // Fixture Teardown
    }

    @Test
    public void agenciaCriacao()
    {
        // Fixture Setup, delegated
        final Banco meuBanco = this.criarBanco();

        // Exercise SUT, delegated
        final Agencia minhaAgencia = meuBanco.criarAgencia( "Minha Agencia" );

        // Result Verification
        Assert.assertNotNull( minhaAgencia );

        // Fixture Teardown
    }

    @Test
    public void agenciaCriacaoNome()
    {
        // Fixture Setup, inline
        final String nomeEsperado = "Minha Agencia";

        // Fixture Setup, delegated
        final Banco meuBanco = this.criarBanco();

        // Exercise SUT, delegated
        final Agencia minhaAgencia = meuBanco.criarAgencia( nomeEsperado );

        // Result Verification
        Assert.assertEquals( nomeEsperado, minhaAgencia.obterNome() );

        // Fixture Teardown
    }

    @Test
    public void verificarNome()
    {
        // Fixture Setup, delegated
        final Banco meuBanco = this.criarBanco();

        // Exercise SUT
        final boolean isCorrectName = meuBanco.obterNome().equals( "Caixa Economica" );

        // Result Verification
        Assert.assertTrue( isCorrectName );

        // Fixture Teardown
    }

    @Test
    public void verificarMoeda()
    {
        // Fixture Setup, delegated
        final Banco meuBanco = this.criarBanco();

        // Exercise SUT
        final boolean isCorrectName = meuBanco.obterMoeda().equals( Moeda.BRL );

        // Result Verification
        Assert.assertTrue( isCorrectName );

        // Fixture Teardown
    }
}
