/**
 * Pacote que contém a classe principal de testes.
 */
package homeBroker;

import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import testes.DriverClass;

/**
 * @author Professional
 *
 */
public class ProgramaPrincipal
{
    /**
     * Está é a única instância da classe principal.
     */
    private static final ProgramaPrincipal INSTANCE = new ProgramaPrincipal();
    
    /**
     * Janela principal que contém a interface gráfica inicial do programa.
     */
    private static JanelaPrincipal janelaPrincipal;
    
    /**
     * 
     */
    private JanelaDoBook janelaDoBook;
    
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
    public ArrayList< Conta > contasTeste;
    
    /**
     * Construtor que inicializa a janela principal.
     */
    private ProgramaPrincipal()
    {
        // Liga o book de ofertas
        this.janelaDoBook = new JanelaDoBook();
        this.processoDoBook = new Thread( this.janelaDoBook );
        this.processoDoBook.start();
        
        // Cria contas fictícias
        this.contasTeste = DriverClass.criarContasFicticia( 150, "123" );
        
        // Login temporário para testes.
        this.contaAutenticada = this.contasTeste.get( 0 );
    }
    
    /**
     * Serve para implementação do padrão de projeto singleton. Retorna a única
     * instancia existe da JanelaPrincipal.
     * 
     * @return INSTANCE a única instancia existe da JanelaPrincipal.
     */
    public static ProgramaPrincipal getInstance()
    {
        return ProgramaPrincipal.INSTANCE;
    }
    
    /**
     * Método principal que inicia a execução do programa. Este programa não
     * reconhece nenhum tipo de argumento.
     * 
     * @param args um array de argumentos do tipo String passados por linha de
     *            comando.
     */
    public static void main( String... args )
    {
        /*
         * Here we are Secheduling a JOB for Event Dispatcher Thread, since
         * Swing is not Thread Safe. This is used to place the code which is
         * responsible for creating and diaplaying your GUI.
         * 
         * Também serve para se livrar o modificador static, assim entrar em um
         * contexto do objeto this.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            @Override
            public void run()
            {
                // Faz login
                ProgramaPrincipal.INSTANCE.loginNoSistema( null );
                
                if( ProgramaPrincipal.INSTANCE.contaAutenticada != null )
                {
                    // Cria uma janela para a aplicação principal.
                    ProgramaPrincipal.janelaPrincipal =
                            new JanelaPrincipal( "HomeBroker Tabajara" );
                }
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
        // case "c":
        // Conta novaConta = Principal.criarUsuario();
        // contasTeste.add( novaConta );
        // break;
        case "m":
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
        this.contaAutenticada = login;
    }
}