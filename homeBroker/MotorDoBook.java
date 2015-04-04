/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

import javax.swing.JOptionPane;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class MotorDoBook implements Runnable
{
    private static final MotorDoBook INSTÂNCIA = new MotorDoBook();
    
    private static boolean DEBUG = false;
    
    private BookDeOfertas bookDeOfertas;
    
    private JanelaDoBook janelaDoBook;
    
    private MotorDoBook()
    {
        if( ProgramaPrincipal.isDebug() || MotorDoBook.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor do MotorDoBook!" );
        }
        this.bookDeOfertas = BookDeOfertas.getInstance();
        this.janelaDoBook = JanelaDoBook.getInstance();
    }
    
    /**
     * @return the instance
     */
    public static MotorDoBook getInstance()
    {
        return INSTÂNCIA;
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
            if( ProgramaPrincipal.isDebug() || MotorDoBook.DEBUG )
            {
                String texto =
                        "Estou em JanelaDoBook chamando o teste \n\n this.bookDeOfertas.existemNovasOfertas( this.janelaDoBook.getNúmeroDeOfertas()"
                                + this.bookDeOfertas
                                        .existemNovasOfertas( this.janelaDoBook
                                                .getNúmeroDeOfertas() );
                JOptionPane.showMessageDialog( null, texto );
            }
            if( this.bookDeOfertas.existemNovasOfertas( this.janelaDoBook
                    .getNúmeroDeOfertas() ) )
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
    
    /**
     * Atualiza a lista de ofertas do book de ofertas.
     */
    private void atualizarListaDeOfertas()
    {
        int indice = this.janelaDoBook.getNúmeroDeOfertas();
        String ofertaDoMercado = this.bookDeOfertas.ofertaToString( indice );
        this.janelaDoBook.adicionarOfertaDeMercado( ofertaDoMercado );
        
        if( ProgramaPrincipal.isDebug() || MotorDoBook.DEBUG )
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
}