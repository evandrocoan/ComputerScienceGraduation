
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

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
        final SistemaBancario sistemaBancario = new SistemaBancario();
        final Banco caixaEconomica = sistemaBancario.criarBanco( "Caixa Economica", Moeda.BRL );
        return caixaEconomica;
    }

    @Test
    public void bancoCriacao()
    {
        // Fixture Setup

        // Exercise SUT
        final Banco meuBanco = this.criarBanco();

        // Result Verification
        Assert.assertNotNull( meuBanco );

        // Fixture Teardown
    }
}
