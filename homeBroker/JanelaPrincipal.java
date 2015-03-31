/**
 * 
 */
package homeBroker;

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
import javax.swing.WindowConstants;

/**
 * 
 * @author Professional
 */
public class JanelaPrincipal extends JFrame
{
    private PainelJanelaPrincipal painelJanelaPrincipal;
    protected JanelaDoBook janelaDoBook;
    
    /**
     * Construtor que cria a janela principal do programa.
     * 
     * @param títuloDaJanela o títuloDaJanela que será criada.
     * @param programaPrincipal uma instância do programaPrincipal para realizar
     *            suas funções
     * @param janelaDoBook uma instância da janelaDoBook para ele poder ser
     *            acessada através do menu principal.
     * 
     */
    public JanelaPrincipal( String títuloDaJanela,
            ProgramaPrincipal programaPrincipal, JanelaDoBook janelaDoBook )
    {
        super( títuloDaJanela );
        this.janelaDoBook = janelaDoBook;
        
        // Cria o painelJanelaPrincipal
        this.painelJanelaPrincipal =
                new PainelJanelaPrincipal( programaPrincipal );
        
        // Adiciona o painelJanelaPrincipal na janelaPrincipal
        this.add( this.painelJanelaPrincipal.getPainelPrincipal() );
        
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
     * Representa o painel principal da janela principal
     * 
     * @author Professional
     */
    private class PainelJanelaPrincipal extends JPanel
    {
        private ProgramaPrincipal programaPrincipal;
        
        private JPanel painelPrincipal;
        private JTextField caixaDeTextoPrincipal;
        private JButton botãoPrincipal;
        private JTextArea comandosDisponíveis;
        
        /**
         * Cria um painel para colocar os botões, caixas de texto, ...
         * 
         * @param programaPrincipal
         */
        public PainelJanelaPrincipal( ProgramaPrincipal programaPrincipal )
        {
            this.programaPrincipal = programaPrincipal;
            
            // Cria os compomentos
            this.painelPrincipal = new JPanel( true );
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
            this.painelPrincipal.add( this.botãoPrincipal, BorderLayout.WEST );
            
            this.painelPrincipal.add( this.caixaDeTextoPrincipal,
                    BorderLayout.SOUTH );
            
            this.painelPrincipal.add( this.comandosDisponíveis,
                    BorderLayout.EAST );
            
            Biblioteca.trocarFontes( this.painelPrincipal, new Font( getName(),
                    Frame.NORMAL, 20 ) );
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
                    PainelJanelaPrincipal.this.programaPrincipal
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
                    if( PainelJanelaPrincipal.this.programaPrincipal == null )
                    {
                        JOptionPane.showMessageDialog( null,
                                "programaPrincipal é null!" );
                    }
                    PainelJanelaPrincipal.this.programaPrincipal
                            .menuPrincipal( caixaDeTextoPrincipal.getText() );
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
