/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

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
   
   private final static Fachada fachada = Fachada.getInstância();
   
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
      
      if( JanelaDoHomebroker.fachada.isAdministradora() )
      {
         this.setLocation( 110, 10 );
         
      } else
      {
         this.setLocation( 250, 10 );
      }
      
      // Abre a janela maximizado
      // this.setExtendedState( Frame.MAXIMIZED_BOTH );
      
      // Ajusta a janela ao tamanho dos elementos.
      this.pack();
      this.setVisible( true );
   }
   
   /**
    * @param nova um boolean true caso precisa destruir a janela anterior e construir uma nova
    *           janela, false caso queira se pegar a janela já existente.
    * @return INSTÂNCIA a instância da janela.
    */
   public static JanelaDoHomebroker getInstância( final boolean nova )
   {
      synchronized( JanelaDoHomebroker.class )
      {
         if( JanelaDoHomebroker.instância == null )
         {
            JanelaDoHomebroker.instância = new JanelaDoHomebroker();
         }
         if( nova )
         {
            JanelaDoHomebroker.instância.finalize();
            JanelaDoHomebroker.instância = new JanelaDoHomebroker();
         }
      }
      return JanelaDoHomebroker.instância;
   }
   
   @Override
   protected void finalize()
   {
      try
      {
         this.setVisible( false );
         JanelaDoHomebroker.instância = null;
         this.painel.finalize();
         this.painel = null;
         super.finalize();
         
      } catch( final Throwable exeption )
      {
         exeption.printStackTrace();
      }
   }
}
