
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;

public class TesteBancoCase1
{
    SistemaBancario sistema;
    Banco bancoDoBrasil;

    @Before
    public void executedBeforeEach()
    {
        this.sistema = new SistemaBancario();
        this.bancoDoBrasil = this.criarBanco();
    }

    private Banco criarBanco()
    {
        return this.sistema.criarBanco( "Banco do Brasil", Moeda.BRL );
    }

    @Test
    public void bancoCriacaoNotNull()
    {
        // Fixture Setup

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( this.bancoDoBrasil );

        // Fixture Teardown
    }

    @Test
    public void bancoCriacaoNome()
    {
        // Fixture Setup

        // Exercise SUT
        final Banco meuBanco = this.criarBanco();

        // Result Verification
        Assert.assertEquals( "Banco do Brasil", meuBanco.obterNome() );

        // Fixture Teardown
    }

    @Test
    public void bancoCriacaoMoeda()
    {
        // Fixture Setup

        // Exercise SUT
        final Banco meuBanco = this.criarBanco();

        // Result Verification
        Assert.assertEquals( Moeda.BRL, meuBanco.obterMoeda() );

        // Fixture Teardown
    }
}
