/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

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
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public final class MonitorDoBook extends JFrame
{
    /**
     * 
     */
    private static final long serialVersionUID = 2978670135770143966L;
    
    /**
     * Resposável por realizar o debug do programa, quando ativado. Deve ser
     * instânciado antes que o construtor desta classe, pois este construtor
     * precisa de deste objeto já instânciado para ser monitorado pelo log.
     */
    private static final Logger LOG = Logger.getLogger(
        MonitorDoBook.class.getName() );
    
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final MonitorDoBook INSTÂNCIA =
        new MonitorDoBook();
    
    private final PainelDoMonitor painelPrincipal;
    
    private MonitorDoBook()
    {
        super( "Monitor do Book De Ofertas" );
        MonitorDoBook.LOG.setLevel( Level.OFF );
        
        if( MonitorDoBook.LOG.isLoggable( Level.SEVERE ) )
        {
            JOptionPane.showMessageDialog( null,
                "Estou no construtor da JanelaDoBook!" );
        }
        if( MonitorDoBook.INSTÂNCIA != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
        
        final Dimension tamanhoDaJanela =
            Toolkit.getDefaultToolkit().getScreenSize();
        final int width = (int) tamanhoDaJanela.getWidth();
        final int height = (int) tamanhoDaJanela.getHeight();
        
        final Dimension tamanhoDaJanelaReduzido =
            new Dimension( width - 100, height - 100 );
        
        this.setSize( tamanhoDaJanelaReduzido );
        this.setPreferredSize( tamanhoDaJanelaReduzido );
        this.setBounds( 50, 50, width - 100, height - 100 );
        this.setVisible( false );
        
        this.painelPrincipal = PainelDoMonitor.getInstância();
        this.setContentPane( this.painelPrincipal );
        
        Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL,
            24 ) );
    }
    
    /**
     * Adiciona uma oferta de mercado ao book de ofertas.
     * 
     * @param ofertaDeMercado uma String representando a oferta de mercado para
     *            se adicionar ao book de ofertas. Os caracteres de quebra de
     *            linhas desta String serão ignorados.
     */
    public void adicionarOfertaDeMercado( final String ofertaDeMercado )
    {
        this.painelPrincipal.modeloPadrãoDeLista.addElement( ofertaDeMercado );
    }
    
    /**
     * @return númeroDeOfertas o número de ofertas já inseridas no book de
     *         ofertas.
     */
    public int getNúmeroDeOfertas()
    {
        return this.painelPrincipal.modeloPadrãoDeLista.getSize();
    }
    
    /**
     * @return the instance
     */
    public static MonitorDoBook getInstance()
    {
        return MonitorDoBook.INSTÂNCIA;
    }
}
