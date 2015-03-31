/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

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
public class JanelaDoBook extends JFrame implements Runnable
{
    private static final JanelaDoBook INSTANCE = new JanelaDoBook();
    private static boolean DEBUG = false;
    private JPanel contentPane;
    
    private BookDeOfertas bookDeOfertas;
    
    private DefaultListModel< String > modeloPadrãoDeLista =
            new DefaultListModel<>();
    
    private JList< String > listaDeOfertas = new JList<>(
            this.modeloPadrãoDeLista );
    
    private JanelaDoBook()
    {
        if( ProgramaPrincipal.DEBUG || JanelaDoBook.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor da JanelaDoBook!" );
        }
        this.bookDeOfertas = BookDeOfertas.getInstance();
        
        this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
        this.setBounds( 500, 500, 500, 500 );
        this.contentPane = new JPanel();
        this.contentPane.setLayout( new GridLayout( 0, 1 ) );
        this.setContentPane( this.contentPane );
        this.configurarJanela();
    }
    
    /**
     * @return the instance
     */
    public static JanelaDoBook getInstance()
    {
        return INSTANCE;
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
            if( ProgramaPrincipal.DEBUG || JanelaDoBook.DEBUG )
            {
                String texto =
                        "Estou em JanelaDoBook chamando o teste \n\n"
                                + "this.bookDeOfertas.existemNovasOfertas( "
                                + "this.modeloPadrãoDeLista.getSize() ) = "
                                + this.bookDeOfertas
                                        .existemNovasOfertas( this.modeloPadrãoDeLista
                                                .getSize() );
                JOptionPane.showMessageDialog( null, texto );
            }
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
        
        Biblioteca.trocarFontes( this, new Font( getName(), Frame.NORMAL, 30 ) );
    }
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarListaDeOfertas()
    {
        int indice = this.modeloPadrãoDeLista.getSize();
        String ofertaDoMercado = this.bookDeOfertas.ofertaToString( indice );
        this.modeloPadrãoDeLista.addElement( ofertaDoMercado );
        
        if( ProgramaPrincipal.DEBUG || JanelaDoBook.DEBUG )
        {
            System.out.println( ofertaDoMercado );
        }
    }
    
    /**
     * 
     */
    public void exibirBookDeOfertas()
    {
        this.setVisible( true );
    }
}
