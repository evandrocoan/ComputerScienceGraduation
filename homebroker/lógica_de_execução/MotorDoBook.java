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
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final MotorDoBook INSTÂNCIA_DO_MOTOR = new MotorDoBook();
    
    private static final Logger LOG = Logger.getLogger( MotorDoBook.class
            .getName() );
    
    /**
     * @return the instance
     */
    public static MotorDoBook getInstance()
    {
        return MotorDoBook.INSTÂNCIA_DO_MOTOR;
    }
    
    private final BookDeOfertas bookDeOfertas;
    
    private final MonitorDoBook janelaDoBook;
    
    private MotorDoBook()
    {
        if( MotorDoBook.LOG.isLoggable( Level.SEVERE ) )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor do MotorDoBook!" );
        }
        if( MotorDoBook.INSTÂNCIA_DO_MOTOR != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        this.bookDeOfertas = BookDeOfertas.getInstância();
        this.janelaDoBook = MonitorDoBook.getInstance();
    }
    
    /**
     * Aciciona um oferta de venda.
     * 
     * @param preço
     * @param quantidade
     * @param açãoAComprar
     * @return true se a oferta foi adicionada com sucesso.
     */
    public boolean adicionarOfertaDeVenda( final double preço,
            final int quantidade, final String açãoAComprar )
    {
        return this.bookDeOfertas.adicionarOfertaDeVenda( preço, quantidade,
                açãoAComprar );
    }
    
    /**
     * 
     */
    public void exibirBookDeOfertas()
    {
        this.janelaDoBook.setVisible( true );
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
                final String texto =
                        "Estou em JanelaDoBook chamando o teste \n\n "
                                + "this.bookDeOfertas.existemNovasOfertas( "
                                + "this.janelaDoBook.getNúmeroDeOfertas()"
                                + this.bookDeOfertas
                                        .existemNovasOfertas( this.janelaDoBook
                                                .getNúmeroDeOfertas() );
                JOptionPane.showMessageDialog( null, texto );
            }
            this.bookDeOfertas.adicionarOfertaDeVenda( 10, 10, "Tabajara SAS" );
            
            if( this.bookDeOfertas.existemNovasOfertas( this.janelaDoBook
                    .getNúmeroDeOfertas() ) )
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
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarListaDeOfertas()
    {
        final int indice = this.janelaDoBook.getNúmeroDeOfertas();
        final String ofertaDoMercado =
                this.bookDeOfertas.ofertaToString( indice );
        this.janelaDoBook.adicionarOfertaDeMercado( ofertaDoMercado );
        
        if( MotorDoBook.LOG.isLoggable( Level.SEVERE ) )
        {
            MotorDoBook.LOG.severe( ofertaDoMercado );
        }
    }
}