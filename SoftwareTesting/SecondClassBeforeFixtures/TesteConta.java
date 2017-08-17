
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.Dinheiro;
import br.ufsc.ine.leb.sistemaBancario.Entrada;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

public class TesteConta
{
    private Agencia agencia;

    @Before
    public void executedBeforeEach()
    {
        final SistemaBancario sistemaBancario = new SistemaBancario();
        final Banco caixaEconomica = sistemaBancario.criarBanco( "Caixa Economica", Moeda.BRL );
        this.agencia = caixaEconomica.criarAgencia( "Agencia" );
    }

    @Test
    public void criarConta()
    {
        // Fixture Setup

        // Exercise SUT
        final Conta minhaConta = this.agencia.criarConta( "Conta Test" );

        // Result Verification
        Assert.assertNotNull( minhaConta );

        // Fixture Teardown
    }

    @Test
    public void realizarTransacao()
    {
        // Fixture Setup
        final Conta saida = this.agencia.criarConta( "Conta Saida" );
        final Conta entrada = this.agencia.criarConta( "Conta Entrada" );

        final Dinheiro quantia = new Dinheiro( Moeda.BRL, 1500, 0 );
        ValorMonetario esperado = new ValorMonetario( Moeda.BRL );

        // Exercise SUT
        esperado = esperado.somar( quantia );
        entrada.adicionarTransacao( new Entrada( saida, quantia ) );

        // Result Verification
        Assert.assertEquals( esperado, entrada.calcularSaldo() );

        // Fixture Teardown
    }
}
