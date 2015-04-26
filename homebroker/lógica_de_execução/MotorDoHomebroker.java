/**
 * 
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.Ação;
import homebroker.lógica_de_dados.Conta;
import homebroker.lógica_de_dados.Inventario;
import homebroker.util.Biblioteca;

import java.util.ArrayList;
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
    private static final MotorDoHomebroker INSTÂNCIA_DO_MOTOR =
        new MotorDoHomebroker();
    
    /**
     * Classe responsável pelo controle da lógica de dados do BookDeOfertas.
     */
    private final MotorDoBook motorDoBook = MotorDoBook.getInstance();
    
    /**
     * As contasTeste que serão utilizadas para simular a adição de contas no
     * sistema, isto é, as contas criadas somente existirão temporariamente.
     */
    private transient List< Conta > contasTeste;
    
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
        super();
        MotorDoHomebroker.LOG.setLevel( Level.OFF );
        
        if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
        {
            MotorDoHomebroker.LOG.severe(
                "Estou no construtor de ProgramaPrincipal()" );
        }
        if( MotorDoHomebroker.INSTÂNCIA_DO_MOTOR != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        // Liga o book de ofertas
        final Thread processoDoBook = new Thread( this.motorDoBook );
        processoDoBook.start();
        
        // Cria contas fictícias
        this.criarContasFicticia( 30, "123" );
    }
    
    /**
     * Transforma um ArrayList de contas e uma String
     * 
     * @param contas um ArrayList contendo as contas
     * @return texto um texto contendo os nomes das contas de teste criadas
     */
    private String contasTesteToString()
    {
        final StringBuffer texto = new StringBuffer();
        
        int contador = 0;
        for( final Conta conta: this.contasTeste )
        {
            texto.append( conta.getNome() );
            texto.append( ", " );
            if( Biblioteca.quebrarLinha( contador ) )
            {
                texto.append( "\n" );
            }
            contador = contador + 1;
        }
        return texto.toString();
    }
    
    /**
     * Cria contas teste para o sistema.
     * 
     * @param quantidade a quantidade de contas teste para se criar
     * @param senha senha que as contas de teste terão
     */
    @SuppressWarnings( "all" )
    private void criarContasFicticia( final int quantidade, final String senha )
    {
        final ArrayList< Conta > contasTeste = new ArrayList<>();
        contasTeste.add( new Conta( "admin", "admin", 2000.5 * Biblioteca
            .gerarNumeroAleatorio(), true, new Inventario() ) );
        
        MotorDoHomebroker.criarInventarioFicticio( contasTeste.get( 0 ),
            quantidade );
        
        Conta contaTeste;
        
        for( int i = 0; i < quantidade; i++ )
        {
            contaTeste = new Conta( "User" + Biblioteca.gerarNumeroAleatorio(),
                senha, 2000.5 * Biblioteca.gerarNumeroAleatorio(), false,
                new Inventario() );
            MotorDoHomebroker.criarInventarioFicticio( contaTeste, quantidade );
            
            contasTeste.add( contaTeste );
        }
        if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
        {
            MotorDoHomebroker.LOG.severe( "Estou em criarContasFictícias "
                + contasTeste.get( 0 ).getNome() );
        }
        this.contasTeste = contasTeste;
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
     * Método de realiza o login no sistema.
     * 
     * @param darDica uma dica que será aprensetada no menu do login.
     *            Inicialmente ela serve para exibir quais contas estão
     *            disponiveis para login e sua senha
     */
    
    public void loginNoSistema( final boolean darDica )
    {
        if( darDica )
        {
            final StringBuilder dica = new StringBuilder();
            dica.append( "\n" );
            dica.append( this.contasTesteToString() );
            this.loginNoSistemaInterno( dica.toString() );
        } else
        {
            this.loginNoSistemaInterno( "" );
        }
    }
    
    /**
     * 
     */
    private boolean loginNoSistemaChecagem( final String usuário,
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
     * 
     */
    private void loginNoSistemaInterno( final String dica )
    {
        String usuário = "";
        String senha = "";
        boolean inputError = true;
        do
        {
            usuário = JOptionPane.showInputDialog(
                ( inputError? "" : "Usuário ou senha inválidos\n\n" )
                + "Insira qual conta será feito login: " + dica );
            
            if( ( usuário == null ) )
            {
                break;
            }
            senha = JOptionPane.showInputDialog( "Insira qual senha para a "
                + "conta: " + usuário );
            
            if( ( senha == null ) )
            {
                break;
            }
            inputError = this.loginNoSistemaChecagem( usuário, senha );
            
        } while( !inputError );
        
        if( ( usuário == null ) | ( senha == null ) )
        {
            System.exit( 0 );
        }
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
     * Cria um inventário fictício de ações contendo 5 ações fictícias.
     * 
     * @param conta a conta que irá receber as ações fictícioas.
     * @param quantidade a quantidade de ações fictícias para se criar.
     */
    @SuppressWarnings( { "all" } )
    private static void criarInventarioFicticio( final Conta conta,
        final int quantidade )
    {
        Ação ação;
        
        for( int i = 0; i < ( quantidade / 5 ); i++ )
        {
            ação = new Ação( 2.2 + Biblioteca.gerarNumeroAleatorio(),
                10 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SA"
                    + Biblioteca.gerarNumeroAleatorio() );
            conta.getInventario().adicionarAoInventario( ação );
            
            ação = new Ação( 22.2 + Biblioteca.gerarNumeroAleatorio(),
                100 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SO"
                    + Biblioteca.gerarNumeroAleatorio() );
            conta.getInventario().adicionarAoInventario( ação );
            
            ação = new Ação( 200.2 + Biblioteca.gerarNumeroAleatorio(),
                1000 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SP"
                    + Biblioteca.gerarNumeroAleatorio() );
            conta.getInventario().adicionarAoInventario( ação );
            
            ação = new Ação( 2000.2 + Biblioteca.gerarNumeroAleatorio(),
                10000 + Biblioteca.gerarNumeroAleatorio(), "Tabajara ST"
                    + Biblioteca.gerarNumeroAleatorio() );
            conta.getInventario().adicionarAoInventario( ação );
            
            ação = new Ação( 200006.2 + Biblioteca.gerarNumeroAleatorio(),
                10000 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SS"
                    + Biblioteca.gerarNumeroAleatorio() );
            conta.getInventario().adicionarAoInventario( ação );
        }
    }
    
    /**
     * Retorna a única instancia existe do MotorDoHomebroker.
     * 
     * @return INSTANCE a única instancia existe da JanelaPrincipal.
     */
    public static MotorDoHomebroker getInstance()
    {
        return MotorDoHomebroker.INSTÂNCIA_DO_MOTOR;
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
