/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import java.awt.Frame;

import javax.swing.JFrame;
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
     * Motor responsável pela lógica da interface gráfica principal do programa.
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
        final PainelDoHomebroker painelPrincipal =
            PainelDoHomebroker.getInstância( this.motor );
        
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
    void enviarCommando( final String comando )
    {
        this.motor.menuPrincipal( comando );
    }
    
    /**
     * @param motor o motor do homebroker.
     * @return instância a instância da janela.
     */
    public static JanelaDoHomebroker
    getInstância( final MotorDoHomebroker motor )
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
}
