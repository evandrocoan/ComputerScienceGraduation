
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
public class TesteBancoCase4
{
    @Fixture private Conta contaMaria;
    @Fixture private SistemaBancario sistema;

    @Test
    public void contaAllTests()
    {
        // Fixture Setup
        final int valor = 10;
        final Dinheiro quantia = Helper.criarDinheiro( valor );

        final ValorMonetario valorEsperado = Helper.criarValorMonetario( valor );
        final EstadosDeOperacao operacaoEsperada = EstadosDeOperacao.SUCESSO;

        // Exercise SUT, inline
        Operacao resultado = this.sistema.depositar( this.contaMaria, quantia );

        // Result Verification
        Assert.assertEquals( operacaoEsperada, resultado.obterEstado() );
        Assert.assertEquals( valorEsperado, this.contaMaria.calcularSaldo() );

        // Fixture Teardown
    }
}
