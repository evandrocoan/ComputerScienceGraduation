
import org.junit.Test;
import junit.framework.TestCase;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import br.ufsc.ine.leb.projetos.estoria.Fixture;
import br.ufsc.ine.leb.projetos.estoria.FixtureSetup;
import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

@FixtureSetup( TesteBancoCase2.class )
public class HamcrestTests
{
    @Test
    public void testEqualsObjectMatcher()
    {
        final String theBiscuit = new String( "Ginger" );
        final String myBiscuit = new String( "Ginger" );

        MatcherAssert.assertThat( theBiscuit, Matchers.equalTo( myBiscuit ) );
    }

    @Test
    public void testEqualsNumberMatcher()
    {
        final int theBiscuit = 5;
        final int myBiscuit = 4;

        MatcherAssert.assertThat( theBiscuit, Matchers.greaterThan( myBiscuit ) );
    }

    @Test
    public void testEqualsTextMatcher()
    {
        final String theBiscuit = new String( "First Part" );
        final String myBiscuit = new String( "Part" );

        MatcherAssert.assertThat( theBiscuit, Matchers.endsWith( myBiscuit ) );
    }

    @Test
    public void testEqualsArrayMatcher()
    {
        final String[] myBiscuit = new String[] {
            "foo", "bar"
        };
        MatcherAssert.assertThat( myBiscuit, Matchers.arrayWithSize( 2 ) );
    }

    @Test
    public void testEqualsAllOfMatcher()
    {
        final String myBiscuit = "myValue";
        MatcherAssert.assertThat( myBiscuit, Matchers.allOf( Matchers.startsWith( "my" ), Matchers.containsString( "Val" ) ) );
    }

    @Fixture
    private Agencia agenciaCentro;

    @Test
    public void contaAllTests()
    {
        final Conta contaMaria = this.agenciaCentro.criarConta( "Maria" );
        final ValorMonetario saldoEsperado = Helper.criarValorMonetario( 0 );

        final String nomeEsperado = "Maria";
        final String identificadorEsperado = "0001-5";

        // Assert.assertNotNull( contaMaria );
        MatcherAssert.assertThat( contaMaria, Matchers.is( Matchers.notNullValue( Conta.class ) ) );

        // Assert.assertEquals( identificadorEsperado,  );
        MatcherAssert.assertThat( identificadorEsperado, Matchers.equalTo( contaMaria.obterIdentificador() ) );

        // Assert.assertEquals( nomeEsperado, contaMaria.obterTitular() );
        MatcherAssert.assertThat( nomeEsperado, Matchers.equalTo( contaMaria.obterTitular() ) );

        // Assert.assertEquals( saldoEsperado, contaMaria.calcularSaldo() );
        MatcherAssert.assertThat( saldoEsperado, Matchers.equalTo( contaMaria.calcularSaldo() ) );

        // Assert.assertEquals( this.agenciaCentro, contaMaria.obterAgencia() );
        MatcherAssert.assertThat( this.agenciaCentro, Matchers.equalTo( contaMaria.obterAgencia() ) );
    }
}
