
import org.junit.Assert;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Dinheiro;
import br.ufsc.ine.leb.sistemaBancario.Moeda;

public class TesteDinheiro
{
    @Test
    public void dinheiroCriacao()
    {
        // Fixture Setup

        // Exercise SUT, inline
        final Dinheiro dinheiro = new Dinheiro( Moeda.BRL, 10, 10 );

        // Result Verification
        Assert.assertNotNull( dinheiro );

        // Fixture Teardown
    }

    @Test
    public void dinheiroDezReal()
    {
        // Fixture Setup

        // Exercise SUT, inline
        final Dinheiro dinheiro = new Dinheiro( Moeda.BRL, 10, 10 );

        // Result Verification
        Assert.assertEquals( 1010, dinheiro.obterQuantiaEmEscala().intValue() );

        // Fixture Teardown
    }

    @Test
    public void dinheiroMoeda()
    {
        // Fixture Setup, inline
        Moeda moeda = Moeda.BRL;

        // Exercise SUT
        final Dinheiro dinheiro = new Dinheiro( moeda, 10, 10 );

        // Result Verification
        Assert.assertEquals( moeda, dinheiro.obterMoeda() );

        // Fixture Teardown
    }
}
