/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.BookDeOfertas;

import javax.swing.JOptionPane;

/**
 * Cuida da execução do BookDeOfertas.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class MotorDoBook implements Runnable
{
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final MotorDoBook INSTÂNCIA_DO_MOTOR = new MotorDoBook();
    
    private static boolean DEBUG = false;
    
    private BookDeOfertas bookDeOfertas;
    
    private MonitorDoBook janelaDoBook;
    
    private MotorDoBook()
    {
        if( MotorDoBook.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor do MotorDoBook!" );
        }
        if( INSTÂNCIA_DO_MOTOR != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        this.bookDeOfertas = BookDeOfertas.getInstance();
        this.janelaDoBook = MonitorDoBook.getInstance();
    }
    
    /**
     * @return the instance
     */
    public static MotorDoBook getInstance()
    {
        return INSTÂNCIA_DO_MOTOR;
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
            if( MotorDoBook.DEBUG )
            {
                String texto =
                        "Estou em JanelaDoBook chamando o teste \n\n this.bookDeOfertas.existemNovasOfertas( this.janelaDoBook.getNúmeroDeOfertas()"
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
            } catch( InterruptedException e )
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
        int indice = this.janelaDoBook.getNúmeroDeOfertas();
        String ofertaDoMercado = this.bookDeOfertas.ofertaToString( indice );
        this.janelaDoBook.adicionarOfertaDeMercado( ofertaDoMercado );
        
        if( MotorDoBook.DEBUG )
        {
            System.out.println( ofertaDoMercado );
        }
    }
    
    /**
     * 
     */
    public void exibirBookDeOfertas()
    {
        this.janelaDoBook.setVisible( true );
    }
    
    /**
     * Aciciona um oferta de venda.
     * 
     * @param preço
     * @param quantidade
     * @param açãoAComprar
     * @return true se a oferta foi adicionada com sucesso.
     */
    public boolean adicionarOfertaDeVenda( double preço, int quantidade,
            String açãoAComprar )
    {
        return this.bookDeOfertas.adicionarOfertaDeVenda( preço, quantidade,
                açãoAComprar );
    }
}