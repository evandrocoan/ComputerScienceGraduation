/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

/**
 * Representa uma oferta de venta ou compra.
 * 
 * @author Professional
 */
public class OfertaDoMercado
{
    private Ação açãoEmOferta;
    private String tipoDeOferta;
    
    /**
     * @param açãoEmOferta
     * @param tipoDeOferta
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
}
