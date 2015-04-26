/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;
import homebroker.util.Biblioteca;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class Homebroker extends JFrame
{
    /**
     * Representa o painel principal da janela principal.
     * 
     * @authors Evandro  Coan, Renan Pinho Assi
     */
    private class PainelPrincipal extends JPanel
    {
        
        /**
         * 
         */
        private static final long serialVersionUID = -4450248854153724051L;
        
        JTextField caixaDeTextoPrincipal;
        JButton botãoPrincipal;
        final JTextArea comandosDisponíveis;
        
        /**
         * Cria um painel para colocar os botões, caixas de texto, ...
         */
        public PainelPrincipal()
        {
            super();
            
            // Cria os compomentos
            this.caixaDeTextoPrincipal = this.caixaDeTextoPrincipal();
            this.botãoPrincipal = this.botãoPrincipal();
            this.comandosDisponíveis =
                    new JTextArea( "Bem-vindo ao sistema "
                            + "tabajara de cadastro de ações!\n"
                            + "Digite 's' para fechar o programa.\n"
                            + "Digite 'v' para para ver o inventario\n"
                            + "Digite 'ov' para enviar uma ordem de venda\n"
                            // + "Digite 'c' para para criar uma conta!\n"
                            + "Digite 'm' para ver o mercado!\n" );
            
            // Configura os componentes
            super.setLayout( new BorderLayout() );
            this.comandosDisponíveis.setEditable( false );
            this.comandosDisponíveis.setFocusable( false );
            
            // Adiciona os componentes ao painel principal
            this.add( this.botãoPrincipal, BorderLayout.WEST );
            
            this.add( this.caixaDeTextoPrincipal, BorderLayout.NORTH );
            
            this.add( this.comandosDisponíveis, BorderLayout.EAST );
            
            Biblioteca.trocarFontes( this, new Font( this.getName(),
                    Frame.NORMAL, 20 ) );
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
            this.botãoPrincipal = new JButton( "Enviar comando" );
            this.botãoPrincipal.addActionListener( new ActionListener()
            {
                /**
                 * Processa o comando na caixa de texto principal.
                 */
                @Override
                public void actionPerformed( final ActionEvent ae )
                {
                    Homebroker.motorDoHomebroker
                    .menuPrincipal( PainelPrincipal.this.caixaDeTextoPrincipal
                            .getText() );
                }
            } );
            
            // Configura o botão principal
            this.botãoPrincipal.setPreferredSize( new Dimension( 250, 35 ) );
            this.botãoPrincipal.setFocusable( false );
            
            return this.botãoPrincipal;
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
            this.caixaDeTextoPrincipal =
                    new JTextField( "  Insira qual seu comando  " );
            this.caixaDeTextoPrincipal.addActionListener( new ActionListener()
            {
                /**
                 * Obtém o texto digito.
                 */
                @Override
                public void actionPerformed( final ActionEvent ae )
                {
                    if( Homebroker.DEBUG )
                    {
                        if( Homebroker.motorDoHomebroker == null )
                        {
                            JOptionPane.showMessageDialog( null,
                                    "motorDoHomebroker é null!" );
                        }
                    }
                    Homebroker.motorDoHomebroker
                    .menuPrincipal( PainelPrincipal.this.caixaDeTextoPrincipal
                                    .getText() );
                }
            } );
            
            // Limpa a caixa de texto ao clicar com o mouse.
            this.caixaDeTextoPrincipal.addMouseListener( new MouseAdapter()
            {
                /**
                 * Limpa a caixa de texto ao clicar com o mouse.
                 */
                @Override
                // @formatter:off
                public
                void mouseClicked( final MouseEvent e )
                {
                    PainelPrincipal.this.caixaDeTextoPrincipal.setText( "" );
                } // @formatter:on
            } );
            
            // Limpa a caixa de texto ao digital algo
            this.caixaDeTextoPrincipal.addKeyListener( new KeyAdapter()
            {
                /**
                 * Limpa a caixa de texto ao entrar mais que 2 caracteres.
                 */
                @Override
                public void keyPressed( final KeyEvent evt )
                {
                    if( ( evt.getKeyCode() != KeyEvent.VK_ENTER )
                            && ( PainelPrincipal.this.caixaDeTextoPrincipal
                                    .getText().length() > 1 ) )
                    {
                        PainelPrincipal.this.caixaDeTextoPrincipal.setText( "" );
                    }
                }
            } );
            
            // Configura a caixaDeTextoPrincipal
            this.caixaDeTextoPrincipal
            .setPreferredSize( new Dimension( 250, 35 ) );
            
            return this.caixaDeTextoPrincipal;
        }
    }
    
    /**
     * 
     */
    private static final long serialVersionUID = -7263112844101186140L;
    
    /**
     * Contém a única instância deste motor.
     */
    static MotorDoHomebroker motorDoHomebroker = MotorDoHomebroker
            .getInstance();
    
    private static final boolean DEBUG = false;
    
    /**
     * Método principal que inicia a execução do programa.
     * 
     * @param args caso receba o argumento 'teste' abre o programa em uma conta
     *            teste.
     */
    public static void main( final String... args )
    {
        // Faz login
        if( ( args == null ) || ( args.length == 0 ) )
        {
            Homebroker.motorDoHomebroker.loginNoSistema( null );
        } else
        {
            for( int i = 0; i < args.length; i++ )
            {
                switch( args[i] )
                {
                case "teste":
                    JOptionPane.showMessageDialog( null, "Sessão de teste!" );
                    break;
                default:
                    break;
                }
            }
        }
        
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @SuppressWarnings( { "unused", "synthetic-access" } )
            @Override
            public void run()
            {
                new Homebroker();
            }
        } );
    }
    
    private final PainelPrincipal painelPrincipal;
    
    /**
     * Construtor que cria a janela principal do programa.
     */
    private Homebroker()
    {
        super( "HomeBroker Tabajara" );
        
        // Cria o painelJanelaPrincipal
        this.painelPrincipal = new PainelPrincipal();
        this.painelPrincipal.setDoubleBuffered( true );
        
        // Adiciona o painelJanelaPrincipal na janelaPrincipal
        this.add( this.painelPrincipal );
        
        // Used to close the JFrame graciously.
        this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
        
        // Abre a janela maximizado
        this.setLocation( 50, 50 );
        this.setExtendedState( Frame.MAXIMIZED_BOTH );
        
        // Ajusta a janela ao tamanho dos elementos.
        this.pack();
        this.setVisible( true );
    }
}
