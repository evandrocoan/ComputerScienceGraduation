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
 * @author Professional
 */
public final class JanelaDoHomebroker extends JFrame
{
   /**
    * Contém a única instância desta classe.
    */
   private static JanelaDoHomebroker instância;
   
   /**
    * Armazenam o painel do homebroker.
    */
   private final PainelDoHomebroker painelPrincipal;
   
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
      
      // Adiciona o painel principal nesta janela
      this.painelPrincipal = PainelDoHomebroker.getInstância( this.motor );
      this.painelPrincipal.setDoubleBuffered( true );
      this.add( this.painelPrincipal );
      
      // Define que a janela deve fechar ao sair.
      this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
      
      // Abre a janela maximizado
      this.setLocation( 50, 50 );
      this.setExtendedState( Frame.MAXIMIZED_BOTH );
      
      // Ajusta a janela ao tamanho dos elementos.
      this.pack();
   }
   
   /**
    * @return instância a instância da janela.
    */
   public static JanelaDoHomebroker getInstância()
   {
      synchronized( JanelaDoHomebroker.class )
      {
         if( JanelaDoHomebroker.instância == null )
         {
            final MotorDoHomebroker motor = MotorDoHomebroker.getInstância();
            JanelaDoHomebroker.instância = new JanelaDoHomebroker( motor );
         }
      }
      return JanelaDoHomebroker.instância;
   }
   
}
