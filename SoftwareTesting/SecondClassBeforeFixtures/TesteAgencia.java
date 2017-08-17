
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
    public void criarCaixaEconomicaTrindade() throws Exception
    {
        // Fixture Setup
        // Exercise SUT
        final Agencia caixaEconomicaTrindade = this.banco.criarAgencia( "Trindade" );
        // Result Verification
        Assert.assertEquals( "001", caixaEconomicaTrindade.obterIdentificador() );
        Assert.assertEquals( "Trindade", caixaEconomicaTrindade.obterNome() );
        // Fixture Teardown
    }

    @Test
    public void criarDuasAgencias() throws Exception
    {
        // Fixture Setup
        final Agencia trindade = this.banco.criarAgencia( "Trindade" );
        final Agencia serrinha = this.banco.criarAgencia( "Serrinha" );
        // Exercise SUT
        // Result Verification
        Assert.assertEquals( "001", trindade.obterIdentificador() );
        Assert.assertEquals( "002", serrinha.obterIdentificador() );
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
