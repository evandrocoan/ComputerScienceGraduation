/**
 * 
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.Conta;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

/**
 * 
 * @author Professional
 */
public final class MotorDoHomebroker
{
    /**
     * Resposável por realizar o debug do programa, quando ativado. Deve ser
     * instânciado antes que o construtor desta classe, pois este construtor
     * precisa de deste objeto já instânciado para ser monitorado pelo log.
     */
    private static final Logger LOG = Logger.getLogger( "MotorDoHomebroker" );
    
    /**
     * Único objeto desta classe.
     */
    private static final MotorDoHomebroker INSTÂNCIA =
        new MotorDoHomebroker();
    
    /**
     * Classe responsável pelo controle da lógica de dados do BookDeOfertas.
     */
    private final MotorDoBook motorDoBook = MotorDoBook.getInstance();
    
    /**
     * As contasTeste que serão utilizadas para simular a adição de contas no
     * sistema, isto é, as contas criadas somente existirão temporariamente.
     */
    public transient List< Conta > contasTeste;
    
    /**
     * A conta para qual se estará operando o inventário e no merdado de ações.
     */
    private transient Conta contaAutenticada;
    
    /**
     * Construtor que inicializa a o motorDoHomebroker e implementa o padrão
     * sigleton. O atributo JanelaPrincipal.janelaPricipal não é inicializado
     * devio a sua construção necessitar de um objeto deste construtor.
     */
    private MotorDoHomebroker()
    {
        MotorDoHomebroker.LOG.setLevel( Level.OFF );
        
        if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
        {
            MotorDoHomebroker.LOG.severe(
                "Estou no construtor de ProgramaPrincipal()" );
        }
        if( MotorDoHomebroker.INSTÂNCIA != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        // Liga o book de ofertas
        final Thread processoDoBook = new Thread( this.motorDoBook );
        processoDoBook.start();
        
        // Cria contas fictícias
        this.contasTeste = UtiliárioDeContas.criarContasFicticia( 30, "123" );
    }
    
    /**
     * Inicia o processo de criação da conta de um usuário do sistema
     * 
     * //@return conta a conta criada
     */
    public void criarUsuario()
    {
        // TODO
        // String nome = JOptionPane.showInputDialog( "Digite seu nome:" );
        // String senha = JOptionPane.showInputDialog( "Digite sua senha:" );
        // Conta conta = new Conta( nome, senha, 0, false, new Inventario() );
        // ( String nome, String senha, double saldo,boolean
        // administrador, Inventario inventario )
        // return conta;
    }
    
    private void efetuarVendaDeAção()
    {
        if( this.contaAutenticada == null )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        boolean sucesso = false;
        
        while( !sucesso )
        {
            final String nome = this.getNomeAçãoParaVenda();
            if( nome == null )
            {
                return;
            }
            final double preço = this.getPreçoAçãoParaVenda( nome );
            if( preço == 0 )
            {
                return;
            }
            final int quantidade = this.getQuantidadeAçãoParaVenda( nome );
            if( quantidade == 0 )
            {
                return;
            }
            sucesso = this.motorDoBook.adicionarOfertaDeVenda( preço,
                quantidade, nome );
        }
        
    }
    
    private void exibirBookDeOfertas()
    {
        if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
        {
            if( this.motorDoBook == null )
            {
                MotorDoHomebroker.LOG.severe( "motorDoBook é null!" );
            }
        }
        this.motorDoBook.exibirBookDeOfertas();
    }
    
    private String getNomeAçãoParaVenda()
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        String açãoParaVender = null;
        
        while( !sucesso )
        {
            açãoParaVender = JOptionPane.showInputDialog( ( nÉsimaVez
                ? "Ação não existênte!\n\n" : "" )
                + "Lista de ações disponíveis "
                + "para venda: \n"
                + this.contaAutenticada.inventarioToString() );
            if( açãoParaVender == null )
            {
                return null;
            }
            sucesso = this.contaAutenticada
                .existeAçãoNoInvetário( açãoParaVender );
            nÉsimaVez = true;
        }
        return açãoParaVender;
    }
    
    private double getPreçoAçãoParaVenda( final String açãoParaVender )
    {
        final String imput = JOptionPane.showInputDialog(
            "Insira o preço da ação:", Double
            .toString( this.contaAutenticada
                    .getAçãoPreço( açãoParaVender ) ) );
        if( imput == null )
        {
            return 0;
        }
        double preço;
        
        preço = Double.parseDouble( imput );
        return preço;
    }
    
    private int getQuantidadeAçãoParaVenda( final String açãoParaVender )
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        int quantidade = 0;
        
        while( !sucesso )
        {
            final String imput = JOptionPane.showInputDialog( ( nÉsimaVez
                ? "Quantidade não existênte!\n\n" : "" )
                + "Insira a quantidade da ação:", Integer
                .toString( this.contaAutenticada
                    .getAçãoQuantidade( açãoParaVender ) ) );
            if( imput == null )
            {
                return 0;
            }
            quantidade = (int) Double.parseDouble( imput );
            sucesso = this.contaAutenticada
                .existeQuantidadeNoInvetário( quantidade );
            nÉsimaVez = true;
        }
        return quantidade;
    }
    
    /**
     * Verifica se as informações de login saõ válidas.
     * 
     * @param usuário o nome de usuário.
     * @param senha a senha ser verificada.
     * @return true caso seja autenticado a conta
     */
    public boolean loginNoSistemaChecagem( final String usuário,
        final String senha )
    {
        for( final Conta conta: this.contasTeste )
        {
            if( conta.getNome().equals( usuário ) && conta.checkSenha( senha ) )
            {
                this.contaAutenticada = conta;
                return true;
            }
        }
        return false;
    }
    
    /**
     * Menu principal que exibe as opções de operação no mercado e na carteira
     * de ações do cliente.
     * 
     * @param commando o comando inserido pelo usuário
     */
    public void menuPrincipal( String commando )
    {
        if( commando == null )
        {
            commando = "s";
        }
        
        switch( commando )
        {
        case "s":
            MotorDoHomebroker.sairDoSistema();
            break;
        case "v":
            this.mostrarInventário();
            break;
        // case "c":
        // TODO
        // this.criarUsuario();
        // break;
        case "ov":
            this.efetuarVendaDeAção();
            break;
        case "m":
            this.exibirBookDeOfertas();
            break;
        default:
            MotorDoHomebroker.imputError();
            break;
        }
    }
    
    private void mostrarInventário()
    {
        if( this.contaAutenticada == null )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        JOptionPane.showMessageDialog( null,
            this.contaAutenticada.inventarioToString() );
    }
    
    /**
     * Retorna a única instancia existe do MotorDoHomebroker.
     * 
     * @return INSTANCE a única instancia existe da JanelaPrincipal.
     */
    public static MotorDoHomebroker getInstância()
    {
        return MotorDoHomebroker.INSTÂNCIA;
    }
    
    private static void imputError()
    {
        JOptionPane.showMessageDialog( null, "Você digitou uma "
            + "opção inválida!\n\n"
            + "Digite 's' para fechar o programa.\n"
            + "Digite 'v' para para ver o inventario\n"
            // +
            // "Digite 'c' para para criar uma conta!\n"
            + "Digite 'm' para ver o mercado!\n" );
    }
    
    private static void sairDoSistema()
    {
        System.exit( 0 );
    }
}
