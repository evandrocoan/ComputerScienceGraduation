/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import java.awt.Frame;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public final class JanelaDoHomebroker extends JFrame
{
    /**
     * Implementa a serialização do swing.
     */
    private static final long serialVersionUID = -7263112844101186140L;
    
    /**
     * Contém a única instância do homebroker.
     */
    private static JanelaDoHomebroker instância;
    
    /**
     * Motor principal do programa.
     */
    private final MotorDoHomebroker motor;
    
    /**
     * Construtor que cria a janela principal do programa.
     */
    private JanelaDoHomebroker( final MotorDoHomebroker motor )
    {
        super( "HomeBroker Tabajara" );
        this.motor = motor;
        
        // Cria o painel principal
        final PainelDoHomebroker painelPrincipal = PainelDoHomebroker.
            getInstância();
        
        painelPrincipal.setDoubleBuffered( true );
        
        // Adiciona o painel principal nesta janela
        this.add( painelPrincipal );
        
        // Define que a janela deve fechar ao sair.
        this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
        
        // Abre a janela maximizado
        this.setLocation( 50, 50 );
        this.setExtendedState( Frame.MAXIMIZED_BOTH );
        
        // Ajusta a janela ao tamanho dos elementos.
        this.pack();
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
    
    /**
     * Chama a janela responsável por realizar venda da ação.
     */
    private void efetuarVendaDeAção()
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
                janelaDeVendas.efetuarVendaDeAção();
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
        // case "c":
        // TODO
        // this.criarUsuario();
        // break;
        case "ov":
            this.efetuarVendaDeAção();
            break;
        case "m":
            this.motor.exibirBookDeOfertas();
            break;
        default:
            JanelaDoHomebroker.imputError();
            break;
        }
    }
    
    /**
     * Exibe o inventário da conta atualmente autenticada.
     */
    public void mostrarInventário()
    {
        if( this.motor.contaAutenticada == null )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        JOptionPane.showMessageDialog( null,
            this.motor.contaAutenticada.inventarioToString() );
    }
    
    /**
     * @param motor o motor do homebroker.
     * @return instância a instância da janela.
     */
    public static JanelaDoHomebroker getInstância(
        final MotorDoHomebroker motor )
    {
        synchronized( JanelaDoHomebroker.class )
        {
            if( JanelaDoHomebroker.instância == null )
            {
                JanelaDoHomebroker.instância = new JanelaDoHomebroker( motor );
            }
        }
        return JanelaDoHomebroker.instância;
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
}
