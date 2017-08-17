
import org.junit.Assert;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

public class TesteValorMonetario
{

    @Test
    public void valorMonetarioCriacao()
    {
        // Fixture Setup
        // Exercise SUT
        final ValorMonetario valor = new ValorMonetario( Moeda.BRL );
        // Result Verification
        Assert.assertNotNull( valor );
        // Fixture Teardown
    }
}
