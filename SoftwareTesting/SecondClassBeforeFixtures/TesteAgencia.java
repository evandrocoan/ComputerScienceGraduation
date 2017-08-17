
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;

public class TesteAgencia
{
    private Banco banco;

    @Before
    public void executedBeforeEach()
    {
        final SistemaBancario sistemaBancario = new SistemaBancario();
        this.banco = sistemaBancario.criarBanco( "Caixa Economica", Moeda.BRL );
    }

    @Test
    public void criarCaixaEconomicaTrindade()
    {
        // Fixture Setup
        final Agencia caixaEconomicaTrindade = this.banco.criarAgencia( "Trindade" );

        // Exercise SUT
        final boolean areEquals = "Trindade".equals( caixaEconomicaTrindade.obterNome() );

        // Result Verification
        Assert.assertTrue( areEquals );

        // Fixture Teardown
    }

    @Test
    public void criarDuasAgenciasCodigo()
    {
        // Fixture Setup
        final Agencia trindade = this.banco.criarAgencia( "Trindade" );
        final Agencia serrinha = this.banco.criarAgencia( "Serrinha" );

        // Exercise SUT
        final boolean areEqualsFirstAgency = "001".equals( trindade.obterIdentificador() );
        final boolean areEqualsSecondAgency = "002".equals( serrinha.obterIdentificador() );

        // Result Verification
        Assert.assertTrue( areEqualsFirstAgency );
        Assert.assertTrue( areEqualsSecondAgency );

        // Fixture Teardown
    }

    @Test
    public void agenciaCriacao()
    {
        // Fixture Setup

        // Exercise SUT
        final Agencia minhaAgencia = this.banco.criarAgencia( "MinhaAgencia" );

        // Result Verification
        Assert.assertNotNull( minhaAgencia );

        // Fixture Teardown
    }
}
