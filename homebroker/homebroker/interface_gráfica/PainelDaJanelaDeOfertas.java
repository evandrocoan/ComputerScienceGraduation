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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 *
 * @author Professional
 */
public final class PainelDaJanelaDeOfertas extends JPanel
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser
    * instanciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instanciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( PainelDaJanelaDeOfertas.class.getName() );
   
   /**
    * Por padrão, este tipo de instanciação é thread safe.
    */
   private static final PainelDaJanelaDeOfertas INSTÂNCIA = new PainelDaJanelaDeOfertas();
   
   private final DefaultListModel< String > modeloPadrãoDeLista = new DefaultListModel<>();
   
   private final JList< String > listaDeOfertas = new JList<>( this.modeloPadrãoDeLista );
   
   /**
    * Construtor do objeto para implementação do padrão de projeto Singleton.
    */
   private PainelDaJanelaDeOfertas()
   {
      PainelDaJanelaDeOfertas.LOG.setLevel( Level.OFF );
      
      if( PainelDaJanelaDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         JOptionPane.showMessageDialog( null,
                  "Estou no construtor do PainelPrincipal da JanelaDoBook!" );
      }
      
      this.setLayout( new GridLayout( 0, 1 ) );
      this.setSize( super.getSize() );
      this.setPreferredSize( super.getSize() );
      this.setVisible( true );
      
      final JScrollPane painelRolável = new JScrollPane( this.listaDeOfertas );
      this.add( painelRolável, BorderLayout.CENTER );
   }
   
   /**
    * @return the instância
    */
   public static PainelDaJanelaDeOfertas getInstância()
   {
      return PainelDaJanelaDeOfertas.INSTÂNCIA;
   }
   
   public void adicionarOferta( final String ofertaDeMercado )
   {
      this.modeloPadrãoDeLista.addElement( ofertaDeMercado );
   }
   
   public int tamanhoDaLista()
   {
      return this.modeloPadrãoDeLista.getSize();
   }
}
