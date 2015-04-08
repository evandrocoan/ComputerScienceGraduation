/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

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

import testes.DriverClass;
import util.Biblioteca;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class JanelaPrincipal extends JFrame
{
    private static MotorDoHomebroker motorDoHomebroker = MotorDoHomebroker
            .getInstance();
    
    private static boolean DEBUG = false;
    
    private PainelJanelaPrincipal painelJanelaPrincipal;
    
    /**
     * Método principal que inicia a execução do programa.
     * 
     * @param args caso receba o argumento 'teste' abre o programa em uma conta
     *            teste.
     */
    public static void main( String... args )
    {
        // Faz login
        if( args == null || args.length == 0 )
        {
            motorDoHomebroker.loginNoSistema( null );
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
        
        /*
         * Here we are Secheduling a JOB for Event Dispatcher Thread, since
         * Swing is not Thread Safe. This is used to place the code which is
         * responsible for creating and diaplaying your GUI.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            @SuppressWarnings( "unused" )
            @Override
            public void run()
            {
                new JanelaPrincipal();
            }
        } );
    }
    
    /**
     * Construtor que cria a janela principal do programa.
     */
    private JanelaPrincipal()
    {
        super( "HomeBroker Tabajara" );
        
        // Cria o painelJanelaPrincipal
        this.painelJanelaPrincipal = new PainelJanelaPrincipal();
        this.painelJanelaPrincipal.setDoubleBuffered( true );
        
        // Adiciona o painelJanelaPrincipal na janelaPrincipal
        this.add( this.painelJanelaPrincipal );
        
        // Used to close the JFrame graciously.
        this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
        
        // Abre a janela maximizado
        this.setLocation( 50, 50 );
        this.setExtendedState( java.awt.Frame.MAXIMIZED_BOTH );
        
        // Ajusta a janela ao tamanho dos elementos.
        this.pack();
        this.setVisible( true );
    }
    
    /**
     * Representa o painel principal da janela principal.
     * 
     * @authors Evandro  Coan, Renan Pinho Assi
     */
    private class PainelJanelaPrincipal extends JPanel
    {
        private JTextField caixaDeTextoPrincipal;
        private JButton botãoPrincipal;
        private JTextArea comandosDisponíveis;
        
        /**
         * Cria um painel para colocar os botões, caixas de texto, ...
         */
        public PainelJanelaPrincipal()
        {
            // Cria os compomentos
            this.caixaDeTextoPrincipal = this.caixaDeTextoPrincipal();
            this.botãoPrincipal = this.botãoPrincipal();
            this.comandosDisponíveis =
                    new JTextArea( "Bem-vindo ao sistema "
                            + "tabajara de cadastro de ações!\n"
                            + "Digite 's' para fechar o programa.\n"
                            + "Digite 'v' para para ver o inventario\n"
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
            
            Biblioteca.trocarFontes( this, new Font( getName(), Frame.NORMAL,
                    20 ) );
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
                    motorDoHomebroker
                            .menuPrincipal( PainelJanelaPrincipal.this.caixaDeTextoPrincipal
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
                    if( DriverClass.isDebug() || JanelaPrincipal.DEBUG )
                    {
                        if( motorDoHomebroker == null )
                        {
                            JOptionPane.showMessageDialog( null,
                                    "motorDoHomebroker é null!" );
                        }
                    }
                    motorDoHomebroker.menuPrincipal( caixaDeTextoPrincipal
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
