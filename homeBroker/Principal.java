/**
 * Pacote que contém a classe principal de testes.
 */
package homeBroker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import testes.DriverClass;

/**
 * @author Professional
 *
 */
public class Principal extends JFrame
{
    private JFrame janelaPrincipal;
    private Thread threadDoBookDeOfertas;
    private String opçõesDeComando;
    private BookDeOfertas bookDeOfertas;
    
    /**
     * A conta para qual se estará operando o inventário e no merdado de ações.
     */
    private Conta conta;
    
    /**
     * As contasTeste que serão utilizadas para simular a adição de contas no
     * sistema, isto é, as contas criadas somente existirão temporariamente.
     */
    private ArrayList< Conta > contasTeste;
    
    /**
     * Configura a janela principal
     */
    public Principal()
    {
        // Cria uma janela para a aplicação principal.
        this.janelaPrincipal = new JFrame( "HomeBroker Tabajara" );
        
        // Liga os motores.
        this.bookDeOfertas = BookDeOfertas.getInstance();
        this.threadDoBookDeOfertas = new Thread( this.bookDeOfertas );
        this.contasTeste = DriverClass.criarContasFicticia( 150, "123" );
        
        // Login temporário para testes.
        this.conta = this.contasTeste.get( 0 );
        
        this.opçõesDeComando =
                new String( "Bem-vindo ao sistema "
                        + "tabajara de cadastro de ações!\n"
                        + "Digite 's' para fechar o programa.\n"
                        + "Digite 'v' para para ver o inventario\n"
                        // + "Digite 'c' para para criar uma conta!\n"
                        + "Digite 'm' para ver o mercado!\n" );
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
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            @Override
            public void run()
            {
                Principal principal = new Principal();
                
                // Faz login
                principal.loginNoSistema( null );
                
                if( principal.conta != null )
                {
                    principal.criarInterfaceGráficaPrincipal();
                }
            }
        } );
    }
    
    /**
     * 
     */
    public void criarInterfaceGráficaPrincipal()
    {
        this.threadDoBookDeOfertas.start();
        
        // Used to close the JFrame graciously.
        this.janelaPrincipal
                .setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
        
        // Used to position the JFrame at the middle of the screen.
        this.janelaPrincipal.setLocationRelativeTo( null );
        
        // Abre o frame maximizado
        this.janelaPrincipal.setLocation( 50, 50 );
        this.janelaPrincipal.setExtendedState( java.awt.Frame.MAXIMIZED_BOTH );
        
        // Cria e adiciona o painelPrincipal na janelaPrincipal
        PainelPrincipal painelPrincipal = new PainelPrincipal();
        this.janelaPrincipal.add( painelPrincipal.getPainelPrincipal() );
        
        // Ajusta a janela ao tamanho dos elementos.
        this.janelaPrincipal.pack();
        this.janelaPrincipal.setVisible( true );
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
            if( this.conta == null )
            {
                JOptionPane.showMessageDialog( this.janelaPrincipal, "Não há "
                        + "nenhuma conta carregada no sistema!" );
                break;
            }
            JOptionPane.showMessageDialog( this.janelaPrincipal, this.conta
                    .getInventario().inventarioToString() );
            break;
        // case "c":
        // Conta novaConta = Principal.criarUsuario();
        // contasTeste.add( novaConta );
        // break;
        case "m":
            this.bookDeOfertas.exibirBookDeOfertas();
            break;
        default:
            JOptionPane.showMessageDialog( this.janelaPrincipal,
                    "Você digitou uma " + "opção inválida!\n\n"
                            + this.opçõesDeComando );
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
        this.conta = login;
    }
    
    /**
     * Representa o painel principal da janela principal
     * 
     * @author Professional
     */
    private class PainelPrincipal extends Principal
    {
        JPanel painelPrincipal;
        JTextField caixaDeTextoPrincipal;
        JButton botãoPrincipal;
        JTextArea comandosDisponíveis;
        
        /**
         * Cria um painel para colocar os botões, caixas de texto, ...
         */
        public PainelPrincipal()
        {
            // Cria os compomentos
            this.painelPrincipal = new JPanel( true );
            this.caixaDeTextoPrincipal = this.caixaDeTextoPrincipal();
            this.botãoPrincipal = this.botãoPrincipal();
            this.comandosDisponíveis = new JTextArea( super.opçõesDeComando );
            
            // Configura os componentes
            super.setLayout( new BorderLayout() );
            this.comandosDisponíveis.setEditable( false );
            this.comandosDisponíveis.setFocusable( false );
            
            // Adiciona os componentes ao painel principal
            this.painelPrincipal.add( this.botãoPrincipal, BorderLayout.WEST );
            
            this.painelPrincipal.add( this.caixaDeTextoPrincipal,
                    BorderLayout.SOUTH );
            
            this.painelPrincipal.add( this.comandosDisponíveis,
                    BorderLayout.EAST );
            
            Biblioteca.trocarFontes( this.painelPrincipal, new Font( getName(),
                    NORMAL, 20 ) );
        }
        
        /**
         * Retorna o paineilPrincipal construído.
         * 
         * @return painelPrincipal o painelPrincipal quem com os componemtes
         *         principais.
         */
        public JPanel getPainelPrincipal()
        {
            return this.painelPrincipal;
        }
        
        /**
         * Cria o botão principal para enviar os comandos da caixa de texto
         * principal
         * 
         * @return botãoPrincipal o botãoPrincipal que envia os comandas da
         *         caixa de texto principal.
         */
        private JButton botãoPrincipal()
        {
            // Button to show the second JFrame.
            JButton botãoPrincipal = new JButton( "Enviar comando" );
            botãoPrincipal.addActionListener( new ActionListener()
            {
                @SuppressWarnings( "unused" )
                @Override
                public void actionPerformed( ActionEvent ae )
                {
                    Principal.this
                            .menuPrincipal( PainelPrincipal.this.caixaDeTextoPrincipal
                                    .getText() );
                }
            } );
            
            // Configura o botão principal
            botãoPrincipal.setPreferredSize( new Dimension( 250, 35 ) );
            botãoPrincipal.setFocusable( false );
            
            return botãoPrincipal;
        }
        
        /**
         * Cria um campo de texto para entrada de comandos para o programa.
         * 
         * @return caixaDeTextoPrincipal a caixaDeTextoPrincial para a entrada
         *         de comandos.
         */
        private JTextField caixaDeTextoPrincipal()
        {
            // Cria um campo de texto para entrada de comandos para o programa
            JTextField caixaDeTextoPrincipal =
                    new JTextField( "  Insira qual seu comando  " );
            caixaDeTextoPrincipal.addActionListener( new ActionListener()
            {
                @SuppressWarnings( "unused" )
                @Override
                public void actionPerformed( ActionEvent ae )
                {
                    Principal.this.menuPrincipal( caixaDeTextoPrincipal
                            .getText() );
                }
            } );
            
            // Limpa a caixa de texto ao clicar com o mouse.
            caixaDeTextoPrincipal.addMouseListener( new MouseAdapter()
            {
                @SuppressWarnings( "unused" )
                @Override
                public void mouseClicked( MouseEvent e )
                {
                    caixaDeTextoPrincipal.setText( "" );
                }
            } );
            
            // Limpa a caixa de texto ao digital algo
            caixaDeTextoPrincipal.addKeyListener( new KeyAdapter()
            {
                @Override
                public void keyPressed( KeyEvent evt )
                {
                    if( evt.getKeyCode() != KeyEvent.VK_ENTER )
                    {
                        caixaDeTextoPrincipal.setText( "" );
                    }
                }
            } );
            
            // Configura a caixaDeTextoPrincipal
            caixaDeTextoPrincipal.setPreferredSize( new Dimension( 250, 35 ) );
            
            return caixaDeTextoPrincipal;
        }
    }
}