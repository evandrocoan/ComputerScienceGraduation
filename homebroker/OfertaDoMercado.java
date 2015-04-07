/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

import testes.DriverClass;

/**
 * Representa uma oferta de venta ou compra.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class OfertaDoMercado
{
    private Ação açãoEmOferta;
    private String tipoDeOferta;
    private static boolean DEBUG = false;
    
    /**
     * @param açãoEmOferta a ação a ser vendida.
     * @param tipoDeOferta o tipo da oferta
     */
    public OfertaDoMercado( Ação açãoEmOferta, String tipoDeOferta )
    {
        this.açãoEmOferta = açãoEmOferta;
        this.tipoDeOferta = tipoDeOferta;
    }
    
    /**
     * @return the açõesEmOferta
     */
    public Ação getAçãoEmOferta()
    {
        return this.açãoEmOferta;
    }
    
    /**
     * @return the tipoDeOferta
     */
    public String getTipoDeOferta()
    {
        return this.tipoDeOferta;
    }
    
    /**
     * @return açãoEmOferta uma String representando uma ação em oferta.
     */
    public String ofertaToString()
    {
        String açãoEmOferta =
                "Ordem de " + this.getTipoDeOferta() + " - Nome da ação: "
                        + this.getAçãoEmOferta().getNome() + " - Preço: "
                        + this.getAçãoEmOferta().getPreço() + " - Quantidade: "
                        + this.getAçãoEmOferta().getQuantidade();
        
        if( DriverClass.isDebug() || OfertaDoMercado.DEBUG )
        {
            System.out.println( açãoEmOferta );
        }
        
        return açãoEmOferta;
    }
}
