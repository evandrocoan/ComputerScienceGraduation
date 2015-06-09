/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

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
public final class JanelaDeOfertas
{
   private static final Logger LOG;
   private static final JanelaDeOfertas INSTÂNCIA;
   
   static
   {
      LOG = Logger.getLogger( JanelaDeOfertas.class.getName() );
      INSTÂNCIA = new JanelaDeOfertas();
   }
   
   private final JFrame janela = new JFrame( "Book De Ofertas" );
   
   private final Fachada fachada;
   private PainelDeOfertas painel;
   
   private JanelaDeOfertas()
   {
      JanelaDeOfertas.LOG.setLevel( Level.OFF );
      this.fachada = Fachada.getInstância();
      
      this.configurarJanela();
   }
   
   public static JanelaDeOfertas getInstância()
   {
      return JanelaDeOfertas.INSTÂNCIA;
   }
   
   /**
    * Atualiza a lista de ofertas do book de ofertas.
    */
   void atualizarListaDeOfertas()
   {
      int indice = this.painel.tamanhoDaLista();
      
      while( true )
      {
         try
         {
            final String ofertaDoMercado = this.fachada.ofertaToString( indice );
            this.painel.adicionarOferta( ofertaDoMercado );
            
         } catch( final Exception e )
         {
            break;
         }
         indice++;
      }
   }
   
   /**
    * 
    */
   private void configurarJanela()
   {
      final Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
      final int width = (int) tamanhoDaJanela.getWidth();
      final int height = (int) tamanhoDaJanela.getHeight();
      final Dimension tamanhoDaJanelaReduzido = new Dimension( width - 100, height - 100 );
      
      this.janela.setSize( tamanhoDaJanelaReduzido );
      this.janela.setPreferredSize( tamanhoDaJanelaReduzido );
      this.janela.setBounds( 10, 365, width - 540, height - 400 );
      this.janela.setVisible( false );
      this.janela.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
      this.painel = PainelDeOfertas.getInstância();
      this.janela.setContentPane( this.painel );
      
      Biblioteca.trocarFontes( this.janela, new Font( this.janela.getName(), Frame.NORMAL, 20 ) );
   }
   
   public void setVisible( final boolean b )
   {
      this.janela.setVisible( b );
   }
}
