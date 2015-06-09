/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
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
   
   /**
    * @return true caso esteja logado no sistema uma conta de administrador.
    */
   public static boolean isAdministradora()
   {
      if( !JanelaDoHomebroker.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há nenhuma conta carregada no sistema!" );
         return false;
      }
      if( !JanelaDoHomebroker.fachada.isAdministradora() )
      {
         JOptionPane.showMessageDialog( null, "Acesso negado! "
            + "Você precisa ter privilégio de administrador." );
         return false;
      }
      return true;
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
