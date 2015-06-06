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
   private final double preço;
   private final int quantidade;
   private final String ação;
   private final String tipoDeOferta;
   private boolean utilidade;
   
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
      this.preço = preço;
      this.quantidade = quantidade;
      this.ação = ação;
      this.tipoDeOferta = tipoDeOferta;
      this.utilidade = true;
      Oferta.LOG.setLevel( Level.OFF );
   }
   
   /**
    * @param ajuste o valor que será ajustado o saldo da conta.
    */
   public void ajustarSaldo( final double ajuste )
   {
      this.conta.ajustarSaldo( ajuste );
   }
   
   /**
    * @return the açõesEmOferta
    */
   public String getAção()
   {
      return this.ação;
   }
   
   /**
    * @return the conta
    */
   public Conta getConta()
   {
      return this.conta;
   }
   
   /**
    * @return o nome da ação deste oferta.
    */
   public String getNome()
   {
      return this.ação;
   }
   
   /**
    * @return a o preço da ação deste oferta.
    */
   public double getPreço()
   {
      return this.preço;
   }
   
   /**
    * @return a quantidade de ações deste oferta.
    */
   public int getQuantidade()
   {
      return this.quantidade;
   }
   
   /**
    * @return the tipoDeOferta
    */
   public String getTipoDeOferta()
   {
      return this.tipoDeOferta;
   }
   
   /**
    * @return true se esta oferta já foi utilizada, false caso contrário.
    */
   public boolean isUtilidade()
   {
      return this.utilidade;
   }
   
   /**
    * @return açãoEmOferta uma String representando uma ação em oferta.
    */
   public String ofertaToString()
   {
      String ordem = String.format( "Ordem de %-6s", this.getTipoDeOferta() );
      
      if( this.getTipoDeOferta().equals( "Venda" ) )
      {
         ordem = ordem + " ";
      }
      final String açãoEmOferta = ordem + " - Nome da ação: " + this.getAção() + " - Preço: "
         + this.getAção() + " - Quantidade: " + this.getQuantidade();
      
      return açãoEmOferta;
   }
   
   /**
    * Marca essa oferta como já computada por uma venda.
    */
   public void setUtilidade()
   {
      this.utilidade = false;
   }
}
