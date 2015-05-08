/**
 * 
 */
package homebroker.lógica_de_execução;

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
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public final class PainelDoMonitor extends JPanel
{
    /**
     * Resposável por realizar o debug do programa, quando ativado. Deve ser
     * instânciado antes que o construtor desta classe, pois este construtor
     * precisa de deste objeto já instânciado para ser monitorado pelo log.
     */
    private static final Logger LOG = Logger.getLogger(
        PainelDoMonitor.class.getName() );
    
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final PainelDoMonitor INSTÂNCIA =
        new PainelDoMonitor();
    
    /**
     * @return the instância
     */
    public static PainelDoMonitor getInstância()
    {
        return PainelDoMonitor.INSTÂNCIA;
    }
    
    final DefaultListModel< String > modeloPadrãoDeLista =
        new DefaultListModel<>();
    
    private final JList< String > listaDeOfertas = new JList<>(
        this.modeloPadrãoDeLista );
    
    /**
     * Construtor do objeto para implementação do padrão de projeto Singleton.
     */
    private PainelDoMonitor()
    {
        PainelDoMonitor.LOG.setLevel( Level.OFF );
        
        if( PainelDoMonitor.LOG.isLoggable( Level.SEVERE ) )
        {
            JOptionPane.showMessageDialog( null,
                "Estou no construtor do PainelPrincipal da JanelaDoBook!" );
        }
        
        this.setLayout( new GridLayout( 0, 1 ) );
        this.setSize( super.getSize() );
        this.setPreferredSize( super.getSize() );
        this.setVisible( true );
        
        final JScrollPane painelRolável =
            new JScrollPane( this.listaDeOfertas );
        this.add( painelRolável, BorderLayout.CENTER );
    }
}
