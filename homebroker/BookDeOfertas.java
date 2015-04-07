/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

import java.util.ArrayList;

import testes.DriverClass;

/**
 * 
 * Inicia a interface gráfica que exibe o book de efertas com as ordens de
 * compra e venda sendo feitas em tempo real.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class BookDeOfertas
{
    /**
     * Por padrão, este tipo de instânciação é thread safe.
     */
    private static final BookDeOfertas INSTANCE = new BookDeOfertas();
    private static boolean DEBUG = false;
    private ArrayList< OfertaDoMercado > ofertasDoMercado;
    
    /**
     * Serve para corrigir um bug no gerador automático de diagramas que não
     * reconhece a composição feita acima do ArrayList de OfertaDoMercado.
     */
    public OfertaDoMercado inútil = new OfertaDoMercado( null, null );
    
    /**
     * Construtor do objeto para implementação do padrão de projeto Singleton.
     */
    private BookDeOfertas()
    {
        this.ofertasDoMercado = new ArrayList<>();
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
     * @param ofertaDoMercado a ofertar do mercado a ser adicionada.
     */
    public void adicionarOfertaDeVenda( OfertaDoMercado ofertaDoMercado )
    {
        this.ofertasDoMercado.add( ofertaDoMercado );
    }
    
    /**
     * Dado o código de uma oferta, informa se existem novas ofertas lançadas no
     * mercado a partir da oferta informada.
     * 
     * @param ultimaOferta a última oferta visualizada
     * @return true se existem novas ofertas, false caso contrário.
     */
    public boolean existemNovasOfertas( int ultimaOferta )
    {
        int númeroDeOfertas = this.ofertasDoMercado.size() - 1;
        
        if( DriverClass.isDebug() || BookDeOfertas.DEBUG )
        {
            System.out.println( "1 - númeroDeOfertas < ultimaOferta = "
                    + ( númeroDeOfertas < ultimaOferta ) + "("
                    + númeroDeOfertas + "<" + ultimaOferta + ")" );
        }
        
        if( númeroDeOfertas < ultimaOferta )
        {
            return false;
        }
        
        if( DriverClass.isDebug() || BookDeOfertas.DEBUG )
        {
            System.out.println( "2 - númeroDeOfertas > ultimaOferta = "
                    + ( númeroDeOfertas > ultimaOferta ) );
        }
        
        return númeroDeOfertas > ultimaOferta;
    }
    
    /**
     * @param indice
     * @return açãoEmOferta uma String representando uma ação em oferta.
     */
    public String ofertaToString( int indice )
    {
        return this.ofertasDoMercado.get( indice ).ofertaToString();
    }
}
