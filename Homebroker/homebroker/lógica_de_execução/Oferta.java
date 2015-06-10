/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;


/**
 * Representa uma oferta de venta ou compra.
 * 
 * @author Professional
 */
public final class Oferta
{
   private final Conta conta;
   private final double preço;
   private final int quantidade;
   private final String ação;
   private final String tipo;
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
      this.conta = conta;
      this.preço = preço;
      this.quantidade = quantidade;
      this.ação = ação;
      this.tipo = tipoDeOferta;
      this.utilidade = true;
   }
   
   /**
    * @param ajuste o valor que será ajustado o saldo da conta.
    */
   public void ajustarSaldo( final double ajuste )
   {
      this.conta.ajustarSaldo( ajuste );
   }
   
   /**
    * @return o nome da ação deste oferta.
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
    * @return o preço da ação deste oferta.
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
    * @return o tipo desta oferta.
    */
   public String getTipoDeOferta()
   {
      return this.tipo;
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
      final String açãoEmOferta = ordem + " - Nome da ação: " + this.ação + " - Preço: "
         + this.preço + " - Quantidade: " + this.getQuantidade();
      
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
