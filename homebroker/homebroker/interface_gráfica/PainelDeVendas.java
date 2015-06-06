/**
 * Pacote principal que contém o Homebroker.
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
public final class PainelDeVendas extends JPanel
{
   private static final Logger LOG;
   private static final PainelDeVendas INSTÂNCIA;
   
   static
   {
      LOG = Logger.getLogger( PainelDeVendas.class.getName() );
      INSTÂNCIA = new PainelDeVendas();
   }
   
   private final DefaultListModel< String > modeloPadrãoDeLista;
   private final JList< String > listaDeVendas;
   
   /**
    * Construtor do objeto para implementação do padrão de projeto Singleton.
    */
   private PainelDeVendas()
   {
      PainelDeVendas.LOG.setLevel( Level.OFF );
      
      this.modeloPadrãoDeLista = new DefaultListModel<>();
      this.listaDeVendas = new JList<>( this.modeloPadrãoDeLista );
      
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
   public static PainelDeVendas getInstância()
   {
      return PainelDeVendas.INSTÂNCIA;
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
