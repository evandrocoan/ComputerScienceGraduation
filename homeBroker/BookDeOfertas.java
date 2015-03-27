/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.GridLayout;
import java.util.ArrayList;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;

/**
 * 
 * Inicia a interface gráfica que exibe o book de efertas com as ordens de
 * compra e venda sendo feitas em tempo real.
 * 
 * @author Professional
 */
public class BookDeOfertas implements Runnable
{
    private static final BookDeOfertas INSTANCE = new BookDeOfertas();
    
    private int ofertasVisualizadas = 0;
    private int ofertasNãoVisualizadas = 0;
    
    private ArrayList< Ação > ações = new ArrayList<>();
    private ArrayList< JLabel > quantidades = new ArrayList<>();
    private ArrayList< JLabel > preços = new ArrayList<>();
    private ArrayList< JLabel > nomes = new ArrayList<>();
    
    private GraphicalUserInterface graphical = new GraphicalUserInterface();
    private JPanel painelPrincipal = new JPanel();
    private JPanel subPainelPrincipal =
            new JPanel( new GridLayout( 4, 0, 2, 2 ) );
    private JLabel jLabelTemp = new JLabel();
    
    /**
     * Construtor do objeto para implementação do padrão de projeto Singleton.
     */
    private BookDeOfertas()
    {
    }
    
    /**
     * Serve para implementação do padrão de projeto singleton.
     * 
     * @return INSTANCE a única instancia existe do BookDeOfertas.
     */
    public static BookDeOfertas getInstance()
    {
        return BookDeOfertas.INSTANCE;
    }
    
    /**
     * Cria uma ordem de venda de uma ação no book de ofertas.
     * 
     * @param ação a ação a ser vendida.
     */
    public void adicionarOfertaDeVenda( Ação ação )
    {
        this.jLabelTemp =
                new JLabel( "Nome: " + ação.getNome(), SwingConstants.CENTER );
        this.nomes.add( this.jLabelTemp );
        
        this.jLabelTemp =
                new JLabel( "Preço: " + ação.getPreço(), SwingConstants.CENTER );
        this.preços.add( this.jLabelTemp );
        
        this.jLabelTemp =
                new JLabel( "Quantidade: " + ação.getQuantidade(),
                        SwingConstants.CENTER );
        this.quantidades.add( this.jLabelTemp );
        
        this.ações.add( ação );
        this.ofertasNãoVisualizadas++;
        // this.atualizarBookDeOfertas();
    }
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarBookDeOfertas()
    {
        this.jLabelTemp =
                new JLabel( this.ações.get( this.ofertasVisualizadas )
                        .getNome(), SwingConstants.CENTER );
        this.subPainelPrincipal.add( this.jLabelTemp );
        
        this.jLabelTemp =
                new JLabel( "Quantidade: "
                        + this.ações.get( this.ofertasVisualizadas )
                                .getQuantidade(), SwingConstants.CENTER );
        this.subPainelPrincipal.add( this.jLabelTemp );
        
        this.jLabelTemp =
                new JLabel(
                        "Preço: R$"
                                + this.ações.get( this.ofertasVisualizadas )
                                        .getPreço(), SwingConstants.CENTER );
        this.subPainelPrincipal.add( this.jLabelTemp );
        
        this.subPainelPrincipal.setBackground( Color.black );
        this.subPainelPrincipal.setBorder( new EmptyBorder( 4, 4, 4, 4 ) );
        this.nomes.get( this.ofertasVisualizadas ).setOpaque( true );
        this.nomes.get( this.ofertasVisualizadas ).setBackground( Color.white );
        this.quantidades.get( this.ofertasVisualizadas ).setOpaque( true );
        this.quantidades.get( this.ofertasVisualizadas ).setBackground(
                Color.white );
        this.preços.get( this.ofertasVisualizadas ).setOpaque( true );
        this.preços.get( this.ofertasVisualizadas ).setBackground( Color.white );
        this.painelPrincipal.add( this.subPainelPrincipal );
        this.ofertasVisualizadas++;
    }
    
    /**
     * Implementa uma thread que atualiza o book de ofertas em intervalos de
     * 1000 milisegundos caso haja mudanças.
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run()
    {
        this.painelPrincipal.setBackground( Color.WHITE );
        this.painelPrincipal.setBounds( 300, 300, 300, 300 );
        this.graphical.add( new Canvas() );
        this.graphical.add( this.painelPrincipal, BorderLayout.CENTER );
        this.graphical.setVisible( true );
        
        while( true )
        {
            System.out.println( "Oi " + this.ofertasNãoVisualizadas );
            
            if( this.ofertasNãoVisualizadas > this.ofertasVisualizadas )
            {
                System.out.println( "TôAquê " + this.ofertasNãoVisualizadas );
                this.atualizarBookDeOfertas();
            }
            try
            {
                Thread.sleep( 1000 );
            } catch( InterruptedException e )
            {
                // TODO
            }
            this.painelPrincipal.validate();
        }
        
    }
}
