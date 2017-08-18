
import br.ufsc.ine.leb.sistemaBancario.Agencia;
import br.ufsc.ine.leb.sistemaBancario.Banco;
import br.ufsc.ine.leb.sistemaBancario.Conta;
import br.ufsc.ine.leb.sistemaBancario.Dinheiro;
import br.ufsc.ine.leb.sistemaBancario.Entrada;
import br.ufsc.ine.leb.sistemaBancario.Moeda;
import br.ufsc.ine.leb.sistemaBancario.Saida;
import br.ufsc.ine.leb.sistemaBancario.SistemaBancario;
import br.ufsc.ine.leb.sistemaBancario.Transacao;
import br.ufsc.ine.leb.sistemaBancario.ValorMonetario;

public class Helper
{
    public static Agencia criarAgencia()
    {
        final SistemaBancario sistemaBancario = new SistemaBancario();
        final Banco caixaEconomica = sistemaBancario.criarBanco( "Caixa Economica", Moeda.BRL );
        return caixaEconomica.criarAgencia( "Agencia" );
    }

    public static Conta criarConta()
    {
        return Helper.criarAgencia().criarConta( "Conta Teste" );
    }

    public static Dinheiro criarDinheiro( final int quantia )
    {
        return new Dinheiro( Moeda.BRL, quantia, 0 );
    }

    public static String criarIdentificador( final int codigo, final String titular )
    {
        return String.format( "%04d-%d", new Integer( codigo ), new Integer( titular.length()
            % 10 ) );
    }

    public static ValorMonetario criarValorMonetario( final int quantia )
    {
        final ValorMonetario valor = new ValorMonetario( Moeda.BRL );
        return valor.somar( Helper.criarDinheiro( quantia ) );
    }

    public static Transacao criarTransacaoSaida( final int quantia )
    {
        return new Saida( Helper.criarConta(), Helper.criarDinheiro( quantia ) );
    }

    public static Transacao criarTransacaoEntrada( final int quantia )
    {
        return new Entrada( Helper.criarConta(), Helper.criarDinheiro( quantia ) );
    }

    public static void adicionarSaldo( final Conta conta, final int quantia )
    {
        final Conta saida = Helper.criarAgencia().criarConta( "Conta Saida" );
        final Dinheiro dinheiro = new Dinheiro( Moeda.BRL, new Integer( quantia ), new Integer( 0 ) );

        conta.adicionarTransacao( new Entrada( saida, dinheiro ) );
    }
}
