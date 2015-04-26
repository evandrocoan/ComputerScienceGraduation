/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

import homebroker.util.Biblioteca;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Toolkit;

import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class MonitorDoBook extends JFrame
{
    /**
     * Classe que constrói a interface gráfica do book de ofertas.
     * 
     * @authors Evandro  Coan, Renan Pinho Assi
     */
    private class PainelPrincipal extends JPanel
    {
        /**
         * 
         */
        private static final long serialVersionUID = 9104840444093828978L;
        
        final DefaultListModel< String > modeloPadrãoDeLista =
                new DefaultListModel<>();
        
        private final JList< String > listaDeOfertas = new JList<>(
                this.modeloPadrãoDeLista );
        
        public PainelPrincipal()
        {
            super();
            
            if( MonitorDoBook.DEBUG )
            {
                JOptionPane
                .showMessageDialog( null,
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
    
    /**
     * 
     */
    private static final long serialVersionUID = 2978670135770143966L;
    
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final MonitorDoBook INSTÂNCIA_DA_JANELA =
            new MonitorDoBook();
    
    private static final boolean DEBUG = false;
    
    /**
     * @return the instance
     */
    public static MonitorDoBook getInstance()
    {
        return MonitorDoBook.INSTÂNCIA_DA_JANELA;
    }
    
    private final PainelPrincipal painelPrincipal;
    
    private MonitorDoBook()
    {
        super( "Monitor do Book De Ofertas" );
        if( MonitorDoBook.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor da JanelaDoBook!" );
        }
        if( MonitorDoBook.INSTÂNCIA_DA_JANELA != null )
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
        
        this.painelPrincipal = new PainelPrincipal();
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
}
