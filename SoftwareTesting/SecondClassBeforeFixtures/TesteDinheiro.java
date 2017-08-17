
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
        // Exercise SUT
        final Dinheiro dinheiro = new Dinheiro( Moeda.BRL, 10, 10 );
        // Result Verification
        Assert.assertNotNull( dinheiro );
        // Fixture Teardown
    }
}
