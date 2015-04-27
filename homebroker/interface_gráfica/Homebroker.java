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
public final class Homebroker extends JFrame
{
    /**
     * Motor responsável pela lógica da interface gráfica principal do programa.
     */
    private static MotorDoHomebroker motor = MotorDoHomebroker
        .getInstância();
    
    /**
     * Motor responsável pela lógica da interface gráfica.
     */
    private static JanelaDeLogin janela = JanelaDeLogin.getInstância(
        Homebroker.motor );
    
    /**
     * Implementa a serialização do swing.
     */
    private static final long serialVersionUID = -7263112844101186140L;
    
    /**
     * Contém a única instância do homebroker.
     */
    private static final Homebroker INSTÂNCIA = new Homebroker();
    
    /**
     * Construtor que cria a janela principal do programa.
     */
    private Homebroker()
    {
        super( "HomeBroker Tabajara" );
        
        // Cria o painel principal
        final PainelDoHomebroker painelPrincipal = PainelDoHomebroker.getInstância(
            Homebroker.motor );
        
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
     * Envia um comando entrado pelo usuário ao interpretador de comandos.
     */
    static void enviarCommando( final String comando )
    {
        Homebroker.motor.menuPrincipal( comando );
    }
    
    /**
     * @return instância a instância de programa principal.
     */
    static Homebroker getInstância()
    {
        return Homebroker.INSTÂNCIA;
    }
    
    /**
     * Realiza a leitura dos parâmetros passados por linha de comando. Caso não
     * haja nenhum parâmetro, inicia a interface gráfica de login no sistema.
     * 
     * @param args "Os comandos disponívels:\n" +
     *            "teste: abre o programa em mode de teste sem dica" +
     *            "das contas para se logar\n" +
     *            "dica: o mesmo que teste, mas abre o programa " +
     *            "com dicas de contas para se logar."
     */
    @SuppressWarnings( "all" )
    private static void iniciarSistema( final String[] args )
    {
        if( ( args == null ) || ( args.length == 0 ) )
        {
            Homebroker.janela.loginNoSistema( "login" );
        } else
        {
            boolean exitLoop = false;
            
            for( int i = 0; i < args.length; i++ )
            {
                switch( args[i] )
                {
                case "teste":
                    JOptionPane.showMessageDialog( null, "Sessão de teste!" );
                    Homebroker.janela.loginNoSistema( "teste" );
                    break;
                    
                case "dica":
                    JOptionPane.showMessageDialog( null, "Sessão de teste "
                        + "COM dica de contas no login!" );
                    Homebroker.janela.loginNoSistema( "dica" );
                    break;
                    
                case "ajuda":
                    System.out.println( "Comandos disponívels:\n"
                        + "teste: abre o programa em mode de teste sem dica"
                        + "das contas para se logar\n"
                        + "dica: o mesmo que teste, mas abre o programa "
                        + "com dicas de contas para se logar." );
                    exitLoop = true;
                    break;
                    
                default:
                    System.out.println( "Linha de Comando inválido! " + args[i] );
                    exitLoop = true;
                    break;
                }
                if( exitLoop )
                {
                    break;
                }
            }
        }
        Homebroker.INSTÂNCIA.setVisible( true );
    }
    
    /**
     * Método principal que inicia a execução do programa.
     * 
     * @param args caso receba o argumento 'teste' abre o programa em uma conta
     *            teste.
     */
    public static void main( final String... args )
    {
        Homebroker.iniciarSistema( args );
        
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
                Homebroker.getInstância();
            }
        } );
    }
}
