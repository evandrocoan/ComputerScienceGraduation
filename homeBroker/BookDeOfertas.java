/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.util.ArrayList;

import javax.swing.JPanel;
import javax.swing.JTextArea;

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
    
    private ArrayList< JTextArea > blocoDeAção;
    private ArrayList< Ação > ações;
    
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
                "Ordem de venda\nNome da ação: " + ação.getNome()
                        + "       \nPreço: " + ação.getPreço()
                        + "\nQuantidade: " + ação.getQuantidade() + "\n";
        
        JTextArea texto = new JTextArea( blocoDeAção );
        texto.setEditable( false );
        texto.setFocusable( true );
        // texto.setPreferredSize( new Dimension( 100, 50 ) );
        
        this.blocoDeAção.add( texto );
        this.ações.add( ação );
        this.ofertasNãoVisualizadas++;
        // this.atualizarBookDeOfertas();
    }
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarBookDeOfertas()
    {
        JTextArea texto = this.blocoDeAção.get( this.ofertasVisualizadas );
        
        this.painelPrincipal.add( texto, new GridLayout() );
        
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
        Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
        int width = (int) tamanhoDaJanela.getWidth();
        int height = (int) tamanhoDaJanela.getHeight();
        Dimension tamanhoDaJanelaReduzido =
                new Dimension( width - 100, height - 100 );
        
        this.painelPrincipal.setSize( tamanhoDaJanelaReduzido );
        this.painelPrincipal.setBounds( 50, 50, width - 100, height - 100 );
        
        this.graphical.setBounds( 50, 50, width - 100, height - 100 );
        
        this.graphical.add( new Canvas() );
        this.graphical.add( this.painelPrincipal, BorderLayout.CENTER );
        this.graphical.setVisible( false );
        
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
}
