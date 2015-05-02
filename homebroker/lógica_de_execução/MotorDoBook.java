/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.BookDeOfertas;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

/**
 * Cuida da execução do BookDeOfertas.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public final class MotorDoBook implements Runnable
{
    /**
     * Resposável por realizar o debug do programa, quando ativado. Deve ser
     * instânciado antes que o construtor desta classe, pois este construtor
     * precisa de deste objeto já instânciado para ser monitorado pelo log.
     */
    private static final Logger LOG = Logger.getLogger( MotorDoBook.class.getName() );
    
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final MotorDoBook INSTÂNCIA = new MotorDoBook();
    
    private final BookDeOfertas bookDeOfertas;
    
    private final MonitorDoBook monitorDoBook;
    
    private MotorDoBook()
    {
        MotorDoBook.LOG.setLevel( Level.OFF );
        
        if( MotorDoBook.LOG.isLoggable( Level.SEVERE ) )
        {
            JOptionPane.showMessageDialog( null,
                "Estou no construtor do MotorDoBook!" );
        }
        if( MotorDoBook.INSTÂNCIA != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        this.bookDeOfertas = BookDeOfertas.getInstância();
        this.monitorDoBook = MonitorDoBook.getInstance();
    }
    
    /**
     * Aciciona um oferta de venda.
     * 
     * @param preço o preço da ação.
     * @param quantidade a quantidade de ações.
     * @param ação o nome da ação.
     * @return true se a oferta foi adicionada com sucesso.
     */
    public boolean adicionarOfertaDeVenda( final double preço,
        final int quantidade, final String ação )
    {
        return this.bookDeOfertas.adicionarOfertaDeVenda( preço, quantidade,
            ação );
    }
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarListaDeOfertas()
    {
        final int indice = this.monitorDoBook.getNúmeroDeOfertas();
        final String ofertaDoMercado = this.bookDeOfertas.ofertaToString( indice );
        this.monitorDoBook.adicionarOfertaDeMercado( ofertaDoMercado );
        
        if( MotorDoBook.LOG.isLoggable( Level.SEVERE ) )
        {
            MotorDoBook.LOG.severe( ofertaDoMercado );
        }
    }
    
    /**
     * 
     */
    public void exibirBookDeOfertas()
    {
        this.monitorDoBook.setVisible( true );
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
            if( MotorDoBook.LOG.isLoggable( Level.SEVERE ) )
            {
                final String texto = "Estou em JanelaDoBook chamando o teste "
                    + "\n\n this.bookDeOfertas.existemNovasOfertas( "
                    + "this.janelaDoBook.getNúmeroDeOfertas()"
                    + this.bookDeOfertas.existemNovasOfertas(
                        this.monitorDoBook.getNúmeroDeOfertas() );
                MotorDoBook.LOG.severe( texto );
            }
            this.bookDeOfertas.adicionarOfertaDeVenda( 10, 10, "Tabajara SAS" );
            
            if( this.bookDeOfertas.existemNovasOfertas( this.monitorDoBook.getNúmeroDeOfertas() ) )
            {
                this.atualizarListaDeOfertas();
            }
            try
            {
                Thread.sleep( 10000 );
            } catch( final InterruptedException e )
            {
                // TODO
            }
        }
    }
    
    /**
     * @return the instance
     */
    public static MotorDoBook getInstance()
    {
        return MotorDoBook.INSTÂNCIA;
    }
}
