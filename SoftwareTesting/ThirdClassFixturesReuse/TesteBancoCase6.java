
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.projetos.estoria.Fixture;
import br.ufsc.ine.leb.projetos.estoria.FixtureSetup;
import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.Dinheiro;
import br.ufsc.ine.leb.sistemaBancario.EstadosDeOperacao;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.Operacao;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

@FixtureSetup( {TesteBancoCase3.class, TesteBancoCase1.class} )
public class TesteBancoCase6
{
    @Fixture private Conta contaMaria;
    @Fixture private SistemaBancario sistema;

    @Before
    public void executedBeforeEach()
    {
        final int valor = 4;
        final Dinheiro quantia = Helper.criarDinheiro( valor );

        this.sistema.depositar( this.contaMaria, quantia );
    }

    @Test
    public void contaAllTests()
    {
        // Fixture Setup
        final int quantiaSacar = 6;
        final int quantiaEsperada = 4;
        final Dinheiro dinheiroSacar = Helper.criarDinheiro( quantiaSacar );

        final ValorMonetario valorEsperado = Helper.criarValorMonetario( quantiaEsperada );
        final EstadosDeOperacao operacaoEsperada = EstadosDeOperacao.SALDO_INSUFICIENTE;

        // Exercise SUT, inline
        Operacao resultado = this.sistema.sacar( this.contaMaria, dinheiroSacar );

        // Result Verification
        Assert.assertEquals( operacaoEsperada, resultado.obterEstado() );
        Assert.assertEquals( valorEsperado, this.contaMaria.calcularSaldo() );

        // Fixture Teardown
    }
}
