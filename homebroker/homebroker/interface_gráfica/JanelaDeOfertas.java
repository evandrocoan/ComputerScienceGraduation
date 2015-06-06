/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.WindowConstants;

import util.Biblioteca;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 *
 * @author Professional
 */
public final class JanelaDeOfertas extends JFrame
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser instanciado antes que o
    * construtor desta classe, pois este construtor precisa de deste objeto já instanciado para ser
    * monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( JanelaDeOfertas.class.getName() );
   
   private static final JanelaDeOfertas INSTÂNCIA = new JanelaDeOfertas();
   private static MotorDoHomebroker motor = MotorDoHomebroker.getInstância();
   private final PainelDaJanelaDeOfertas painelPrincipal;
   
   private JanelaDeOfertas()
   {
      super( "Book De Ofertas" );
      JanelaDeOfertas.LOG.setLevel( Level.OFF );
      
      final Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
      final int width = (int) tamanhoDaJanela.getWidth();
      final int height = (int) tamanhoDaJanela.getHeight();
      final Dimension tamanhoDaJanelaReduzido = new Dimension( width - 100, height - 100 );
      
      this.setSize( tamanhoDaJanelaReduzido );
      this.setPreferredSize( tamanhoDaJanelaReduzido );
      this.setBounds( 10, 365, width - 620, height - 400 );
      this.setVisible( false );
      this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
      
      this.painelPrincipal = PainelDaJanelaDeOfertas.getInstância();
      this.setContentPane( this.painelPrincipal );
      
      Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL, 18 ) );
   }
   
   /**
    * @return the instance
    */
   public static JanelaDeOfertas getInstância()
   {
      return JanelaDeOfertas.INSTÂNCIA;
   }
   
   /**
    * Atualiza a lista de ofertas do book de ofertas.
    */
   void atualizarListaDeOfertas()
   {
      int indice = this.painelPrincipal.tamanhoDaLista();
      
      while( true )
      {
         try
         {
            final String ofertaDoMercado = JanelaDeOfertas.motor.ofertaToString( indice );
            this.painelPrincipal.adicionarOferta( ofertaDoMercado );
            
         } catch( final Exception e )
         {
            break;
         }
         indice++;
      }
   }
}
