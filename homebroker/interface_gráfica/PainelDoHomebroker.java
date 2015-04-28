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
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

/**
 * Representa o painel principal da janela principal.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public final class PainelDoHomebroker extends JPanel
{
    /**
     * Implementa a serialização do swing.
     */
    private static final long serialVersionUID = -4450248854153724051L;
    
    private static JanelaDoHomebroker janela =
        JanelaDoHomebroker.getInstância( MotorDoHomebroker.getInstância() );
    
    /**
     * Contém a única instância do painel.
     */
    private static PainelDoHomebroker instância;
    
    /**
     * Campo onde para entrada de comandos para o programa em forma de texto.
     */
    private transient JTextField entradaDeComandos;
    
    /**
     * Botão que envia ao usuário os comandos que estão no campo
     * entradaDeComandos.
     */
    private transient JButton botãoDeComandos;
    
    /**
     * Cria um painel para colocar os botões, caixas de texto, ...
     */
    private PainelDoHomebroker()
    {
        // Configura os compomentos
        this.configurarEntradaDeComandos();
        this.configurarBotãoDeComandos();
        
        /**
         * Contém as informações de apresentação e como utilizar o programa.
         * Tais informações serão aprentadas na interface gráfica ao usuário.
         */
        final JTextArea campoDeAjuda = new JTextArea( "Bem-vindo ao sistema "
            + "tabajara de cadastro de ações!\n"
            + "Digite 's' para fechar o programa.\n"
            + "Digite 'v' para para ver o inventario\n"
            + "Digite 'ov' para enviar uma ordem de venda\n"
            // + "Digite 'c' para para criar uma conta!\n"
            + "Digite 'm' para ver o mercado!\n" );
        campoDeAjuda.setEditable( false );
        campoDeAjuda.setFocusable( false );
        
        // Define o gerênciador de layout utilizado.
        super.setLayout( new BorderLayout() );
        
        // Adiciona os componentes ao painel principal
        this.add( this.botãoDeComandos, BorderLayout.WEST );
        this.add( this.entradaDeComandos, BorderLayout.NORTH );
        this.add( campoDeAjuda, BorderLayout.EAST );
        
        Biblioteca.trocarFontes( this, new Font( this.getName(),
            Frame.NORMAL, 20 ) );
    }
    
    /**
     * Cria o botão principal para enviar os comandos da caixa de texto
     * principal.
     */
    private void configurarBotãoDeComandos()
    {
        this.botãoDeComandos = new JButton( "Enviar comando" );
        this.botãoDeComandos.addActionListener( new ActionListener()
        {
            /**
             * Processa o comando na caixa de texto principal
             */
            @Override
            public void actionPerformed( final ActionEvent ae )
            {
                PainelDoHomebroker.this.enviarCommando( ae.getActionCommand() );
            }
        } );
        
        // Configura o botão principal
        this.botãoDeComandos.setPreferredSize( new Dimension( 250, 35 ) );
        this.botãoDeComandos.setFocusable( false );
    }
    
    /**
     * Configura o campo de texto para entrada de comandos para o programa.
     */
    private void configurarEntradaDeComandos()
    {
        this.entradaDeComandos =
            new JTextField( "  Insira qual seu comando  " );
        
        this.entradaDeComandos.addActionListener( new ActionListener()
        {
            /**
             * Obtém o texto digitado.
             */
            @Override
            public void actionPerformed( final ActionEvent ae )
            {
                PainelDoHomebroker.this.enviarCommando( ae.getActionCommand() );
            }
        } );
        
        this.entradaDeComandos.addMouseListener( new MouseAdapter()
        {
            /**
             * Limpa a caixa de texto ao clicar com o mouse.
             */
            @Override
            public void mouseClicked( final MouseEvent e )
            {
                PainelDoHomebroker.this.limpar();
            }
        } );
        
        this.entradaDeComandos.addKeyListener( new KeyAdapter()
        {
            /**
             * Limpa a caixa de texto ao entrar mais que 2 caracteres.
             */
            @Override
            public void keyPressed( final KeyEvent evt )
            {
                if( ( evt.getKeyCode() != KeyEvent.VK_ENTER )
                    && ( PainelDoHomebroker.this.conteúdo().length() > 1 ) )
                {
                    PainelDoHomebroker.this.limpar();
                }
            }
        } );
        
        // Configura a caixaDeTextoPrincipal
        this.entradaDeComandos.setPreferredSize( new Dimension( 250, 35 ) );
    }
    
    /**
     * @return conteúdo o conteúdo da caixa de texto principal.
     */
    String conteúdo()
    {
        return this.entradaDeComandos.getText();
    }
    
    /**
     * Envia um comando entrado pelo usuário ao interpretador de comandos.
     */
    void enviarCommando( final String comando )
    {
        PainelDoHomebroker.janela.enviarCommando( comando );
    }
    
    /**
     * Limpa o conteúdo da caixa de texto principal.
     */
    void limpar()
    {
        this.entradaDeComandos.setText( "" );
    }
    
    /**
     * @return instância uma intância da janela de login.
     */
    public static PainelDoHomebroker getInstância()
    {
        synchronized( PainelDoHomebroker.class )
        {
            if( PainelDoHomebroker.instância == null )
            {
                PainelDoHomebroker.instância = new PainelDoHomebroker();
            }
        }
        return PainelDoHomebroker.instância;
    }
}
