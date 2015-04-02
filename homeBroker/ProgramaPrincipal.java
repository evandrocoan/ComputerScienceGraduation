/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import testes.DriverClass;

/**
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class ProgramaPrincipal
{
    /**
     * Está é a única instância da classe principal.
     */
    private static ProgramaPrincipal INSTANCE;
    
    /**
     * Janela principal que contém a interface gráfica inicial do programa.
     */
    private static JanelaPrincipal janelaPrincipal;
    
    /**
     * 
     */
    private static JanelaDoBook janelaDoBook;
    
    /**
     * Processo que mantém o book de ofertas funcionando enquanto a interface
     * trabalha.
     */
    private Thread processoDoBook;
    
    /**
     * A conta para qual se estará operando o inventário e no merdado de ações.
     */
    private Conta contaAutenticada;
    
    /**
     * As contasTeste que serão utilizadas para simular a adição de contas no
     * sistema, isto é, as contas criadas somente existirão temporariamente.
     */
    private ArrayList< Conta > contasTeste;
    
    /**
     * Define se o programa executará em mode de DEBUG.
     */
    public static final boolean DEBUG = false;
    
    /**
     * Construtor que inicializa a o programa principal e implementa o padrão
     * sigleton. O atributo JanelaPrincipal.janelaPricipal não é inicializado
     * devio a sua construção necessitar de um objeto deste construtor.
     */
    private ProgramaPrincipal()
    {
        if( ProgramaPrincipal.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor de ProgramaPrincipal()" );
        }
        
        // Liga o book de ofertas
        ProgramaPrincipal.janelaDoBook = JanelaDoBook.getInstance();
        this.processoDoBook = new Thread( ProgramaPrincipal.janelaDoBook );
        this.processoDoBook.start();
        
        // Cria contas fictícias
        this.contasTeste = DriverClass.criarContasFicticia( 30, "123" );
        
        // Login temporário para testes.
        this.contaAutenticada = this.contasTeste.get( 0 );
        
        // Cria ofertas de compra e venda para fíctícias
        DriverClass.testarBookDeOfertas( this.contasTeste,
                BookDeOfertas.getInstance() );
    }
    
    /**
     * @return janelaPrincipal a janelaPrincipal deste programa.
     */
    private static JanelaPrincipal getJanelaPrincipal()
    {
        if( janelaPrincipal == null )
        {
            janelaPrincipal =
                    new JanelaPrincipal( "HomeBroker Tabajara",
                            ProgramaPrincipal.getInstance(), janelaDoBook );
        }
        return janelaPrincipal;
    }
    
    /**
     * Serve para implementação do padrão de projeto singleton. Retorna a única
     * instancia existe da JanelaPrincipal.
     * 
     * @return INSTANCE a única instancia existe da JanelaPrincipal.
     */
    private static ProgramaPrincipal getInstance()
    {
        if( ProgramaPrincipal.INSTANCE == null )
        {
            return new ProgramaPrincipal();
        }
        janelaPrincipal = getJanelaPrincipal();
        return ProgramaPrincipal.INSTANCE;
    }
    
    /**
     * Método principal que inicia a execução do programa. Este programa não
     * reconhece nenhum tipo de argumento.
     * 
     * @param args caso receba o argumento 'teste' abre o programa em uma conta
     *            teste.
     */
    public static void main( String... args )
    {
        /*
         * Here we are Secheduling a JOB for Event Dispatcher Thread, since
         * Swing is not Thread Safe. This is used to place the code which is
         * responsible for creating and diaplaying your GUI.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            @Override
            public void run()
            {
                ProgramaPrincipal programaPrincipal =
                        ProgramaPrincipal.getInstance();
                
                // Faz login
                if( args == null || args.length == 0 )
                {
                    programaPrincipal.loginNoSistema( null );
                } else
                {
                    for( int i = 0; i < args.length; i++ )
                    {
                        switch( args[i] )
                        {
                        case "teste":
                            System.out.println( "Sessão de teste!" );
                            break;
                        default:
                            break;
                        }
                    }
                }
                
                // Cria uma janela para a aplicação principal.
                ProgramaPrincipal.janelaPrincipal =
                        new JanelaPrincipal( "HomeBroker Tabajara",
                                programaPrincipal, janelaDoBook );
            }
        } );
    }
    
    /**
     * Inicia o processo de criação da conta de um usuário do sistema
     * 
     * @return conta a conta criada
     */
    public Conta criarUsuario()
    {
        // TODO
        String nome = JOptionPane.showInputDialog( "Digite seu nome:" );
        String senha = JOptionPane.showInputDialog( "Digite sua senha:" );
        Conta conta = new Conta( nome, senha, 0, false, new Inventario() );
        // ( String nome, String senha, double saldo,boolean
        // administrador, Inventario inventario )
        return conta;
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
            System.exit( 0 );
            break;
        case "v":
            if( this.contaAutenticada == null )
            {
                JOptionPane.showMessageDialog(
                        ProgramaPrincipal.janelaPrincipal, "Não há "
                                + "nenhuma conta carregada no sistema!" );
                break;
            }
            JOptionPane.showMessageDialog( ProgramaPrincipal.janelaPrincipal,
                    this.contaAutenticada.getInventario().inventarioToString() );
            break;
        // TODO
        // case "c":
        // Conta novaConta = Principal.criarUsuario();
        // contasTeste.add( novaConta );
        // break;
        case "m":
            if( ProgramaPrincipal.DEBUG )
            {
                if( ProgramaPrincipal.janelaPrincipal == null )
                {
                    JOptionPane.showMessageDialog( null,
                            "janelaPrincipal é null!" );
                }
                if( ProgramaPrincipal.janelaPrincipal.janelaDoBook == null )
                {
                    JOptionPane
                            .showMessageDialog( null, "janelaDoBook é null!" );
                }
            }
            ProgramaPrincipal.janelaPrincipal.janelaDoBook
                    .exibirBookDeOfertas();
            break;
        default:
            JOptionPane.showMessageDialog( ProgramaPrincipal.janelaPrincipal,
                    "Você digitou uma " + "opção inválida!\n\n"
                            + "Digite 's' para fechar o programa.\n"
                            + "Digite 'v' para para ver o inventario\n"
                            // + "Digite 'c' para para criar uma conta!\n"
                            + "Digite 'm' para ver o mercado!\n" );
            break;
        }
        
    }
    
    /**
     * Método de realiza o login no sistema.
     * 
     * @param dica uma dica que será aprensetada no menu do login. Inicialmente
     *            ela serve para exibir quais contas estão disponiveis para
     *            login e sua senha
     */
    public void loginNoSistema( String dica )
    {
        Conta login = null;
        String command = " ", usuario = " ", senha = " ";
        boolean inputError = false;
        
        if( dica != null )
        {
            if( dica.equals( "" ) )
            {
                dica = "(" + dica + ")";
            }
        } else
        {
            dica = "";
        }
        
        while( !command.equals( "sair" ) && !usuario.equals( "sair" )
                && !senha.equals( "sair" ) )
        {
            usuario =
                    JOptionPane.showInputDialog( ( inputError
                            ? "Usuário ou senha inválidos\n\n" : "" )
                            + "Insira qual conta será feito login: " + dica );
            if( usuario == null )
            {
                break;
            }
            inputError = false;
            senha =
                    JOptionPane.showInputDialog( "Insira qual senha "
                            + "para a conta: " + usuario );
            
            if( senha == null )
            {
                break;
            }
            
            for( Conta conta: this.contasTeste )
            {
                if( conta.getNome().equals( usuario ) )
                {
                    command = "sa";
                }
                if( conta.checkSenha( senha ) )
                {
                    command += "ir";
                    
                    if( command.equalsIgnoreCase( "sair" ) )
                    {
                        login = conta;
                        break;
                    }
                }
            }
            inputError = true;
        }
        if( login == null )
        {
            System.exit( 0 );
        }
        this.contaAutenticada = login;
    }
}