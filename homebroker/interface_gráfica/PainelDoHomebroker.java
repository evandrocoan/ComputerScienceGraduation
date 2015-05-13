package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import util.Biblioteca;

/**
 * Representa o painel principal da janela principal.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public final class PainelDoHomebroker extends JPanel
{
    /**
     * Contém a única instância do painel.
     */
    private static PainelDoHomebroker instância;
    
    /**
     * @param motor o motor do programa.
     * @return instância uma intância da janela de login.
     */
    public static PainelDoHomebroker getInstância( final MotorDoHomebroker motor )
    {
        synchronized( PainelDoHomebroker.class )
        {
            if( PainelDoHomebroker.instância == null )
            {
                PainelDoHomebroker.instância = new PainelDoHomebroker( motor );
            }
        }
        return PainelDoHomebroker.instância;
    }
    
    /**
     * Comtém o motor principal.
     */
    private final MotorDoHomebroker motor;
    
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
     * Contém as informações de apresentação e como utilizar o programa. Tais
     * informações serão aprentadas na interface gráfica ao usuário.
     */
    private final String campoDeAjuda = "Bem-vindo ao sistema "
        + "tabajara de cadastro de ações.\n"
        + "Digite 's' para fechar o programa.\n"
        + "Digite 'v' para para ver o inventario.\n"
        + "Digite 'b' para para bloquerar uma conta de usuário.\n"
        + "Digite 'ov' para enviar uma ordem de venda.\n"
        + "Digite 'oc' para criar um ordem de compra.\n"
        + "Digite 'c' para para criar uma conta.\n"
        + "Digite 'm' para ver o mercado.\n";
    
    /**
     * Cria um painel para colocar os botões, caixas de texto, ...
     */
    private PainelDoHomebroker( final MotorDoHomebroker motor )
    {
        this.motor = motor;
        
        // Configura os compomentos
        this.configurarEntradaDeComandos();
        this.configurarBotãoDeComandos();
        
        final JTextArea campoDeAjuda = new JTextArea( this.campoDeAjuda );
        campoDeAjuda.setEditable( false );
        campoDeAjuda.setFocusable( false );
        
        // Define o gerênciador de layout utilizado.
        super.setLayout( new BorderLayout() );
        
        // Adiciona os componentes ao painel principal
        this.add( this.botãoDeComandos, BorderLayout.WEST );
        this.add( this.entradaDeComandos, BorderLayout.NORTH );
        this.add( campoDeAjuda, BorderLayout.EAST );
        
        Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL,
            20 ) );
    }
    
    private void bloquearUmUsuário()
    {
        // encapsula o motor para evitar o synthetic-access
        final MotorDoHomebroker motor = this.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeBloqueio janelaDeCadastro;
                janelaDeCadastro = JanelaDeBloqueio.getInstância( motor );
                janelaDeCadastro.efetuarBloqueio();
            }
        } );
    }
    
    /**
     * Inicia o processo de criação da conta de um usuário do sistema
     * 
     * //@return conta a conta criada
     */
    public void cadastrarUsuário()
    {
        // encapsula o motor para evitar o synthetic-access
        final MotorDoHomebroker motor = this.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeCadastro janela;
                janela = JanelaDeCadastro.getInstância( motor );
                janela.efetuarCadastro();
            }
        } );
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
        this.entradaDeComandos = new JTextField( "  Insira qual seu comando  " );
        
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
     * Chama a janela responsável por realizar venda da ação.
     */
    private void efetuarCompra()
    {
        // encapsula o motor para evitar o synthetic-access
        final MotorDoHomebroker motor = this.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeCompras janelaDeCompras;
                janelaDeCompras = JanelaDeCompras.getInstância( motor );
                janelaDeCompras.efetuarCompra();
            }
        } );
    }
    
    /**
     * Chama a janela responsável por realizar venda da ação.
     */
    private void efetuarVenda()
    {
        // encapsula o motor para evitar o synthetic-access
        final MotorDoHomebroker motor = this.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeVendas janelaDeVendas;
                janelaDeVendas = JanelaDeVendas.getInstância( motor );
                janelaDeVendas.efetuarVenda();
            }
        } );
    }
    
    /**
     * Menu principal que exibe as opções de operação no mercado e na carteira
     * de ações do cliente.
     */
    protected void enviarCommando( String comando )
    {
        if( comando == null )
        {
            comando = "s";
        }
        
        switch( comando )
        {
        case "s":
            MotorDoHomebroker.sairDoSistema();
            break;
        case "v":
            this.mostrarInventário();
            break;
        case "b":
            this.bloquearUmUsuário();
            break;
        case "c":
            this.cadastrarUsuário();
            break;
        case "ov":
            this.efetuarVenda();
            break;
        case "oc":
            this.efetuarCompra();
            break;
        case "m":
            this.motor.exibirBookDeOfertas();
            break;
        default:
            this.imputError();
            break;
        }
    }
    
    private void imputError()
    {
        JOptionPane.showMessageDialog( null, "Você digitou uma "
            + "opção inválida!\n\n" + this.campoDeAjuda );
    }
    
    /**
     * Limpa o conteúdo da caixa de texto principal.
     */
    void limpar()
    {
        this.entradaDeComandos.setText( "" );
    }
    
    /**
     * Exibe o inventário da conta atualmente autenticada.
     */
    public void mostrarInventário()
    {
        if( !this.motor.isAutenticada() )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        JOptionPane.showMessageDialog( null, this.motor.inventarioToString() );
    }
}
