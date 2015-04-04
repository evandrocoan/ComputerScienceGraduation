/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

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
public class JanelaDoBook extends JFrame
{
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final JanelaDoBook INSTÂNCIA_DA_JANELA = new JanelaDoBook();
    
    private static boolean DEBUG = false;
    
    private PainelPrincipal painelPrincipal;
    
    private JanelaDoBook()
    {
        if( Homebroker.isDebug() || JanelaDoBook.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor da JanelaDoBook!" );
        }
        if( INSTÂNCIA_DA_JANELA != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
        
        Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
        int width = (int) tamanhoDaJanela.getWidth();
        int height = (int) tamanhoDaJanela.getHeight();
        
        Dimension tamanhoDaJanelaReduzido =
                new Dimension( width - 100, height - 100 );
        
        this.setSize( tamanhoDaJanelaReduzido );
        this.setPreferredSize( tamanhoDaJanelaReduzido );
        this.setBounds( 50, 50, width - 100, height - 100 );
        this.setVisible( false );
        
        this.painelPrincipal = new PainelPrincipal();
        this.setContentPane( this.painelPrincipal );
        
        Biblioteca.trocarFontes( this, new Font( getName(), Frame.NORMAL, 24 ) );
    }
    
    /**
     * @return the instance
     */
    public static JanelaDoBook getInstance()
    {
        return INSTÂNCIA_DA_JANELA;
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
     * Adiciona uma oferta de mercado ao book de ofertas.
     * 
     * @param ofertaDeMercado uma String representando a oferta de mercado para
     *            se adicionar ao book de ofertas. Os caracteres de quebra de
     *            linhas desta String serão ignorados.
     */
    public void adicionarOfertaDeMercado( String ofertaDeMercado )
    {
        this.painelPrincipal.modeloPadrãoDeLista.addElement( ofertaDeMercado );
    }
    
    /**
     * Classe que constrói a interface gráfica do book de ofertas.
     * 
     * @authors Evandro  Coan, Renan Pinho Assi
     */
    private class PainelPrincipal extends JPanel
    {
        private DefaultListModel< String > modeloPadrãoDeLista =
                new DefaultListModel<>();
        
        private JList< String > listaDeOfertas = new JList<>(
                this.modeloPadrãoDeLista );
        
        private PainelPrincipal()
        {
            if( Homebroker.isDebug() || JanelaDoBook.DEBUG )
            {
                JOptionPane
                        .showMessageDialog( null,
                                "Estou no construtor do PainelPrincipal da JanelaDoBook!" );
            }
            
            this.setLayout( new GridLayout( 0, 1 ) );
            this.setSize( super.getSize() );
            this.setPreferredSize( super.getSize() );
            this.setVisible( true );
            
            JScrollPane painelRolável = new JScrollPane( this.listaDeOfertas );
            this.add( painelRolável, BorderLayout.CENTER );
        }
    }
}
