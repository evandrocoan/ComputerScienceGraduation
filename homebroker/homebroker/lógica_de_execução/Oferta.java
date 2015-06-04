/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Representa uma oferta de venta ou compra.
 * 
 * @author Professional
 */
public class Oferta
{
   private static final Logger LOG = Logger.getLogger( Oferta.class.getName() );
   
   private final Conta conta;
   private final Ação açãoEmOferta;
   private final String tipoDeOferta;
   
   /**
    * @param preço o preço da ação.
    * @param quantidade a quantidade de ações.
    * @param ação o nome da ação.
    * @param tipoDeOferta o tipo da oferta
    * @param conta a conta qual faz a oferta
    */
   public Oferta( final double preço, final int quantidade, final String ação,
      final String tipoDeOferta, final Conta conta )
   {
      Oferta.LOG.setLevel( Level.OFF );
      
      this.conta = conta;
      this.açãoEmOferta = new Ação( preço, quantidade, ação );
      this.tipoDeOferta = tipoDeOferta;
      Oferta.LOG.setLevel( Level.OFF );
   }
   
   /**
    * @return the açõesEmOferta
    */
   public Ação getAção()
   {
      return this.açãoEmOferta;
   }
   
   /**
    * @return the conta
    */
   public Conta getConta()
   {
      return this.conta;
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
      final String açãoEmOferta =
         "Ordem de " + this.getTipoDeOferta() + " - Nome da ação: " + this.getAção().getNome()
            + " - Preço: " + this.getAção().getPreço() + " - Quantidade: "
            + this.getAção().getQuantidade();
      
      if( Oferta.LOG.isLoggable( Level.SEVERE ) )
      {
         Oferta.LOG.severe( açãoEmOferta );
      }
      
      return açãoEmOferta;
   }
}
