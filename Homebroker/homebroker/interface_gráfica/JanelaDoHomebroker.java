/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

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
   private static JanelaDoHomebroker INSTÂNCIA;
   
   /**
    * Armazenam o painel do homebroker.
    */
   private PainelDoHomebroker painel;
   
   /**
    * Construtor que cria a janela principal do programa.
    */
   private JanelaDoHomebroker()
   {
      super( "Simulador de HomeBroker" );
      
      // Adiciona o painel principal nesta janela
      this.painel = new PainelDoHomebroker();
      this.painel.setDoubleBuffered( true );
      this.add( this.painel );
      
      // Define que a janela deve fechar ao sair.
      this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
      
      // Abre a janela maximizado
      this.setLocation( 110, 10 );
      // this.setLocation( 250, 150 );
      // this.setExtendedState( Frame.MAXIMIZED_BOTH );
      
      // Ajusta a janela ao tamanho dos elementos.
      this.pack();
      this.setVisible( true );
   }
   
   /**
    * @param nova true se precisa construir uma nova janela.
    * @return instância a instância da janela.
    */
   public static JanelaDoHomebroker getInstância( final boolean nova )
   {
      synchronized( JanelaDoHomebroker.class )
      {
         if( ( JanelaDoHomebroker.INSTÂNCIA == null ) || nova )
         {
            JanelaDoHomebroker.INSTÂNCIA = new JanelaDoHomebroker();
         }
      }
      return JanelaDoHomebroker.INSTÂNCIA;
   }
   
   @Override
   protected void finalize()
   {
      try
      {
         this.setVisible( false );
         JanelaDoHomebroker.INSTÂNCIA = null;
         this.painel.finalize();
         this.painel = null;
         super.finalize();
         
      } catch( final Throwable exeption )
      {
         exeption.printStackTrace();
      }
   }
}
