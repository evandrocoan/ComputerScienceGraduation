/**
 * Pacote principal que contém o Homebroker.
 */
package homebrokerr;

import java.util.ArrayList;

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
     * @param ação a ação a ser vendida.
     */
    public void adicionarOfertaDeVenda( Ação ação )
    {
        OfertaDoMercado ofertaDoMercado = new OfertaDoMercado( ação, "Venda" );
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
        
        if( Homebroker.isDebug() || BookDeOfertas.DEBUG )
        {
            System.out.println( "1 - númeroDeOfertas < ultimaOferta = "
                    + ( númeroDeOfertas < ultimaOferta ) + "("
                    + númeroDeOfertas + "<" + ultimaOferta + ")" );
        }
        
        if( númeroDeOfertas < ultimaOferta )
        {
            return false;
        }
        
        if( Homebroker.isDebug() || BookDeOfertas.DEBUG )
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
        OfertaDoMercado ofertaDoMercado = this.ofertasDoMercado.get( indice );
        
        String açãoEmOferta =
                "Ordem de " + ofertaDoMercado.getTipoDeOferta()
                        + " - Nome da ação: "
                        + ofertaDoMercado.getAçãoEmOferta().getNome()
                        + " - Preço: "
                        + ofertaDoMercado.getAçãoEmOferta().getPreço()
                        + " - Quantidade: "
                        + ofertaDoMercado.getAçãoEmOferta().getQuantidade();
        
        if( Homebroker.isDebug() || BookDeOfertas.DEBUG )
        {
            System.out.println( açãoEmOferta );
        }
        
        return açãoEmOferta;
    }
}
