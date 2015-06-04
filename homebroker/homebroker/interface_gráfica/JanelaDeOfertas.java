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
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

import util.Biblioteca;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 *
 * @author Professional
 */
public final class JanelaDeOfertas extends JFrame implements Runnable
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser
    * instanciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instanciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( JanelaDeOfertas.class.getName() );
   
   /**
    * Por padrão, este tipo de instanciação é thread safe.
    */
   private static final JanelaDeOfertas INSTÂNCIA = new JanelaDeOfertas();
   
   private static MotorDoHomebroker motor = MotorDoHomebroker.getInstância();
   
   private final PainelDaJanelaDeOfertas painelPrincipal;
   
   private JanelaDeOfertas()
   {
      super( "Monitor do Book De Ofertas" );
      JanelaDeOfertas.LOG.setLevel( Level.OFF );
      
      if( JanelaDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         JOptionPane.showMessageDialog( null, "Estou no construtor da JanelaDoBook!" );
      }
      if( JanelaDeOfertas.INSTÂNCIA != null )
      {
         throw new IllegalStateException( "Objeto já instanciado!" );
      }
      this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
      
      final Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
      final int width = (int) tamanhoDaJanela.getWidth();
      final int height = (int) tamanhoDaJanela.getHeight();
      
      final Dimension tamanhoDaJanelaReduzido = new Dimension( width - 100, height - 100 );
      
      this.setSize( tamanhoDaJanelaReduzido );
      this.setPreferredSize( tamanhoDaJanelaReduzido );
      this.setBounds( 50, 50, width - 100, height - 100 );
      this.setVisible( false );
      
      this.painelPrincipal = PainelDaJanelaDeOfertas.getInstância();
      this.setContentPane( this.painelPrincipal );
      
      Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL, 24 ) );
   }
   
   /**
    * @return the instance
    */
   public static JanelaDeOfertas getInstância()
   {
      return JanelaDeOfertas.INSTÂNCIA;
   }
   
   /**
    * Adiciona uma oferta de mercado ao book de ofertas.
    *
    * @param ofertaDeMercado uma String representando a oferta de mercado para
    *           se adicionar ao book de ofertas. Os caracteres de quebra de
    *           linhas desta String serão ignorados.
    */
   public void adicionarOfertaDeMercado( final String ofertaDeMercado )
   {
      this.painelPrincipal.adicionarOferta( ofertaDeMercado );
   }
   
   /**
    * Atualiza a lista de ofertas do book de ofertas.
    */
   private void atualizarListaDeOfertas()
   {
      int indice = this.getNúmeroDeOfertas();
      
      while( true )
      {
         try
         {
            final String ofertaDoMercado = JanelaDeOfertas.motor.ofertaToString( indice );
            this.adicionarOfertaDeMercado( ofertaDoMercado );
            
            if( JanelaDeOfertas.LOG.isLoggable( Level.SEVERE ) )
            {
               JanelaDeOfertas.LOG.severe( ofertaDoMercado );
            }
         } catch( final Exception e )
         {
            break;
         }
         indice++;
      }
   }
   
   /**
    * @return númeroDeOfertas o número de ofertas já inseridas no book de
    *         ofertas.
    */
   public int getNúmeroDeOfertas()
   {
      return this.painelPrincipal.tamanhoDaLista();
   }
   
   /**
    * Implementa uma thread que atualiza o book de ofertas em intervalos de 1000
    * milisegundos caso haja mudanças.
    *
    * @see java.lang.Runnable#run()
    */
   @Override
   public void run()
   {
      while( true )
      {
         if( JanelaDeOfertas.LOG.isLoggable( Level.SEVERE ) )
         {
            final String texto =
                     "Estou em JanelaDoBook chamando o teste "
                              + "\n\n this.bookDeOfertas.existemNovasOfertas( "
                              + "this.janelaDoBook.getNúmeroDeOfertas()"
                              + JanelaDeOfertas.motor.existemNovasOfertas( this
                                       .getNúmeroDeOfertas() );
            JanelaDeOfertas.LOG.severe( texto );
         }
         
         JanelaDeOfertas.motor.adicionarOfertaDeVenda( 10, 10, "Tabajara SAS" );
         
         if( JanelaDeOfertas.motor.existemNovasOfertas( this.getNúmeroDeOfertas() ) )
         {
            this.atualizarListaDeOfertas();
         }
         try
         {
            Thread.sleep( 10000 );
         } catch( final InterruptedException e )
         {
            e.printStackTrace();
         }
      }
   }
}
