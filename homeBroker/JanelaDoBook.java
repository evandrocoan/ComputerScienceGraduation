package homeBroker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;

import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 * 
 * @author Professional
 */
public class JanelaDoBook extends JFrame implements Runnable
{
    private JPanel contentPane;
    
    private BookDeOfertas bookDeOfertas;
    
    private DefaultListModel< String > modeloPadrãoDeLista =
            new DefaultListModel<>();
    
    private JList< String > listaDeOfertas = new JList<>(
            this.modeloPadrãoDeLista );
    
    /**
     * 
     */
    public JanelaDoBook()
    {
        this.bookDeOfertas = BookDeOfertas.getInstance();
        
        this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
        this.setBounds( 500, 500, 500, 500 );
        this.contentPane = new JPanel();
        this.contentPane.setLayout( new GridLayout( 0, 1 ) );
        this.setContentPane( this.contentPane );
        this.configurarJanela();
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
        while( true )
        {
            // JOptionPane
            // .showMessageDialog(
            // null,
            // "Estou em JanelaDoBook chamando o teste \n\n"
            // + "this.bookDeOfertas.existemNovasOfertas( "
            // + "this.modeloPadrãoDeLista.getSize() ) = "
            // + this.bookDeOfertas
            // .existemNovasOfertas( this.modeloPadrãoDeLista
            // .getSize() ) );
            if( this.bookDeOfertas
                    .existemNovasOfertas( this.modeloPadrãoDeLista.getSize() ) )
            {
                this.atualizarListaDeOfertas();
            }
            try
            {
                Thread.sleep( 200 );
            } catch( InterruptedException e )
            {
                // TODO
            }
        }
    }
    
    private void configurarJanela()
    {
        Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
        int width = (int) tamanhoDaJanela.getWidth();
        int height = (int) tamanhoDaJanela.getHeight();
        
        Dimension tamanhoDaJanelaReduzido =
                new Dimension( width - 100, height - 100 );
        
        this.setSize( tamanhoDaJanelaReduzido );
        this.setPreferredSize( tamanhoDaJanelaReduzido );
        this.setBounds( 50, 50, width - 100, height - 100 );
        this.setVisible( false );
        
        JScrollPane painelRolável = new JScrollPane( this.listaDeOfertas );
        this.add( painelRolável, BorderLayout.CENTER );
    }
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarListaDeOfertas()
    {
        int indice = this.modeloPadrãoDeLista.getSize();
        String ofertaDoMercado = this.bookDeOfertas.ofertaToString( indice );
        this.modeloPadrãoDeLista.addElement( ofertaDoMercado );
        System.out.println( ofertaDoMercado );
    }
    
    /**
     * 
     */
    public void exibirBookDeOfertas()
    {
        this.setVisible( true );
    }
}
