
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.projetos.estoria.Fixture;
import br.ufsc.ine.leb.projetos.estoria.FixtureSetup;
import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;

@FixtureSetup( TesteBancoCase1.class )
public class TesteBancoCase2
{
    @Fixture private Banco bancoDoBrasil;
    private Agencia agenciaCentro;

    @Before
    public void executedBeforeEach()
    {
        // Helper class
        this.agenciaCentro = this.bancoDoBrasil.criarAgencia( "Centro" );
    }

    @Test
    public void agenciaCriacao()
    {
        // Fixture Setup

        // Exercise SUT
        final Agencia agenciaCentro = this.agenciaCentro;

        // Result Verification
        Assert.assertNotNull( agenciaCentro );

        // Fixture Teardown
    }

    @Test
    public void agenciaIndentificador()
    {
        // Fixture Setup
        final String identificadorEsperado = "001";

        // Exercise SUT
        final Agencia agenciaCentro = this.agenciaCentro;

        // Result Verification
        Assert.assertEquals( identificadorEsperado, agenciaCentro.obterIdentificador() );

        // Fixture Teardown
    }

    @Test
    public void agenciaNome()
    {
        // Fixture Setup
        final String identificadorEsperado = "Centro";

        // Exercise SUT
        final Agencia agenciaCentro = this.agenciaCentro;

        // Result Verification
        Assert.assertEquals( identificadorEsperado, agenciaCentro.obterNome() );

        // Fixture Teardown
    }

    @Test
    public void agenciaNomeDoBanco()
    {
        // Fixture Setup
        final String identificadorEsperado = "Banco do Brasil";

        // Exercise SUT
        final Agencia agenciaCentro = this.agenciaCentro;

        // Result Verification
        Assert.assertEquals( identificadorEsperado, agenciaCentro.obterBanco().obterNome() );

        // Fixture Teardown
    }
}
