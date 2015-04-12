/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_dados;

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
    private static final BookDeOfertas INSTÂNCIA_DO_BOOK = new BookDeOfertas();
    private static boolean DEBUG = false;
    private ArrayList< OfertaDoMercado > ofertasDoMercado;
    
    /**
     * Serve para corrigir um bug no gerador automático de diagramas que não
     * reconhece a composição feita acima do ArrayList de OfertaDoMercado.
     */
    public OfertaDoMercado inútil = new OfertaDoMercado( 0, 0, null, null );
    
    /**
     * Construtor do objeto para implementação do padrão de projeto Singleton.
     */
    private BookDeOfertas()
    {
        if( INSTÂNCIA_DO_BOOK != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        this.ofertasDoMercado = new ArrayList<>();
    }
    
    /**
     * Serve para implementação do padrão de projeto singleton.
     * 
     * @return INSTANCE a única instancia existe do BookDeOfertas.
     */
    public static BookDeOfertas getInstance()
    {
        return BookDeOfertas.INSTÂNCIA_DO_BOOK;
    }
    
    /**
     * Cria uma ordem de venda de uma ação no book de ofertas.
     * 
     * @param preço
     * @param quantidade
     * @param açãoAComprar
     * @return true se a oferta foi adicionada com sucesso.
     */
    public boolean adicionarOfertaDeVenda( double preço, int quantidade,
            String açãoAComprar )
    {
        return this.ofertasDoMercado.add( new OfertaDoMercado( preço,
                quantidade, açãoAComprar, "Venda" ) );
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
