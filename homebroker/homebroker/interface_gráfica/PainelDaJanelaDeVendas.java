/**
 * 
 */
package homebroker.interface_gráfica;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 *
 * @author Professional
 */
public final class PainelDaJanelaDeVendas extends JPanel
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser
    * instanciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instanciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( PainelDaJanelaDeVendas.class.getName() );
   
   /**
    * Por padrão, este tipo de instanciação é thread safe.
    */
   private static final PainelDaJanelaDeVendas INSTÂNCIA = new PainelDaJanelaDeVendas();
   
   private final DefaultListModel< String > modeloPadrãoDeLista = new DefaultListModel<>();
   private final JList< String > listaDeVendas = new JList<>( this.modeloPadrãoDeLista );
   
   /**
    * Construtor do objeto para implementação do padrão de projeto Singleton.
    */
   private PainelDaJanelaDeVendas()
   {
      PainelDaJanelaDeVendas.LOG.setLevel( Level.OFF );
      
      this.setLayout( new GridLayout( 0, 1 ) );
      this.setSize( super.getSize() );
      this.setPreferredSize( super.getSize() );
      this.setVisible( true );
      
      final JScrollPane painelRolável = new JScrollPane( this.listaDeVendas );
      this.add( painelRolável, BorderLayout.CENTER );
   }
   
   /**
    * @return the instância
    */
   public static PainelDaJanelaDeVendas getInstância()
   {
      return PainelDaJanelaDeVendas.INSTÂNCIA;
   }
   
   public void adicionarVenda( final String ofertaDeMercado )
   {
      this.modeloPadrãoDeLista.addElement( ofertaDeMercado );
   }
   
   public int tamanhoDaLista()
   {
      return this.modeloPadrãoDeLista.getSize();
   }
}
