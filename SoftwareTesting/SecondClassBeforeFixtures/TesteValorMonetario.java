
import org.junit.Assert;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Dinheiro;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

public class TesteValorMonetario
{
    @Test
    public void valorMonetarioCriacao()
    {
        // Fixture Setup

        // Exercise SUT, inline
        final ValorMonetario valor = new ValorMonetario( Moeda.BRL );

        // Result Verification
        Assert.assertNotNull( valor );

        // Fixture Teardown
    }

    @Test
    public void valorMonetarioMoeda()
    {
        // Fixture Setup, inline
        final Moeda moeda = Moeda.BRL;

        // Exercise SUT
        final ValorMonetario valor = new ValorMonetario( moeda );

        // Result Verification
        Assert.assertEquals( moeda, valor.obterQuantia().obterMoeda() );

        // Fixture Teardown
    }

    @Test
    public void valorMonetarioQuantia()
    {
        // Fixture Setup, inline
        ValorMonetario valor = new ValorMonetario( Moeda.BRL );

        // Fixture Setup, delegated
        final Dinheiro esperado = Helper.criarDinheiro( 5000 );

        // Exercise SUT
        valor = valor.somar( esperado );

        // Result Verification
        Assert.assertEquals( esperado, valor.obterQuantia() );

        // Fixture Teardown
    }
}
