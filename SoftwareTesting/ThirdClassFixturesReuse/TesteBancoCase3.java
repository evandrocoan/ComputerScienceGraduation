
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.projetos.estoria.Fixture;
import br.ufsc.ine.leb.projetos.estoria.FixtureSetup;
import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

@FixtureSetup( TesteBancoCase2.class )
public class TesteBancoCase3
{
    @Fixture private Agencia agenciaCentro;
    private Conta contaMaria;

    @Before
    public void executedBeforeEach()
    {
        // Helper class
        this.contaMaria = this.agenciaCentro.criarConta( "Maria" );
    }

    @Test
    public void contaAllTests()
    {
        // Fixture Setup
        final Conta contaMaria = this.contaMaria;
        final ValorMonetario saldoEsperado = Helper.criarValorMonetario( 0 );

        final String nomeEsperado = "Maria";
        final String identificadorEsperado = "0001-5";

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( contaMaria );
        Assert.assertEquals( identificadorEsperado, contaMaria.obterIdentificador() );
        Assert.assertEquals( nomeEsperado, contaMaria.obterTitular() );
        Assert.assertEquals( saldoEsperado, contaMaria.calcularSaldo() );
        Assert.assertEquals( this.agenciaCentro, contaMaria.obterAgencia() );

        // Fixture Teardown
    }
}
