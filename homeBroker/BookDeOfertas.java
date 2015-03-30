/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.util.ArrayList;

import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

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
    
    private int ofertasVisualizadas;
    private int ofertasNãoVisualizadas;
    
    private GraphicalUserInterface graphical;
    private JPanel painelPrincipal;
    
    private ArrayList< String > blocoDeAção;
    private ArrayList< Ação > ações;
    
    private DefaultListModel< String > model;
    private JList< String > list;
    
    /**
     * Construtor do objeto para implementação do padrão de projeto Singleton.
     */
    private BookDeOfertas()
    {
        this.ofertasVisualizadas = 0;
        this.ofertasNãoVisualizadas = 0;
        
        this.graphical = new GraphicalUserInterface();
        this.painelPrincipal = new JPanel();
        this.blocoDeAção = new ArrayList<>();
        this.ações = new ArrayList<>();
        
        this.model = new DefaultListModel<>();
        this.list = new JList<>( this.model );
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
        String blocoDeAção =
                "Ordem de venda - Nome da ação: " + ação.getNome()
                        + " - Preço: " + ação.getPreço() + " - Quantidade: "
                        + ação.getQuantidade();
        
        this.blocoDeAção.add( blocoDeAção );
        this.ações.add( ação );
        this.ofertasNãoVisualizadas++;
        // this.atualizarBookDeOfertas();
    }
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarBookDeOfertas()
    {
        int indice = this.ofertasVisualizadas;
        String ação = this.blocoDeAção.get( indice );
        
        this.model.addElement( ação );
        
        this.ofertasVisualizadas++;
    }
    
    /**
     * 
     */
    public void exibirBookDeOfertas()
    {
        this.graphical.setVisible( true );
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
        this.configurarJanelas();
        
        while( true )
        {
            if( this.ofertasNãoVisualizadas > this.ofertasVisualizadas )
            {
                this.atualizarBookDeOfertas();
            }
            try
            {
                Thread.sleep( 200 );
            } catch( InterruptedException e )
            {
                // TODO
            }
            this.painelPrincipal.validate();
        }
    }
    
    private void configurarJanelas()
    {
        Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
        int width = (int) tamanhoDaJanela.getWidth();
        int height = (int) tamanhoDaJanela.getHeight();
        
        Dimension tamanhoDaJanelaReduzido =
                new Dimension( width - 100, height - 100 );
        
        this.painelPrincipal.setSize( tamanhoDaJanelaReduzido );
        this.painelPrincipal.setPreferredSize( tamanhoDaJanelaReduzido );
        this.painelPrincipal.setBounds( 50, 50, width - 100, height - 100 );
        
        this.graphical.setSize( tamanhoDaJanelaReduzido );
        this.graphical.setPreferredSize( tamanhoDaJanelaReduzido );
        this.graphical.setBounds( 50, 50, width - 100, height - 100 );
        
        JScrollPane painelRolável = new JScrollPane( this.list );
        painelRolável
                .setHorizontalScrollBarPolicy( ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED );
        painelRolável.setAutoscrolls( true );
        
        this.graphical.add( painelRolável, BorderLayout.CENTER );
        this.graphical.setVisible( false );
    }
    
    /**
     * @author Professional
     *
     */
    private class GraphicalUserInterface extends JFrame
    {
        private JPanel contentPane;
        
        /**
         * 
         */
        public GraphicalUserInterface()
        {
            this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
            this.setBounds( 500, 500, 500, 500 );
            this.contentPane = new JPanel();
            this.contentPane.setLayout( new GridLayout( 0, 1 ) );
            this.setContentPane( this.contentPane );
        }
    }
}
