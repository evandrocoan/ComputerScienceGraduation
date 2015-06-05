/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

/**
 * 
 * @author Professional
 */
public class Venda
{
   private final Oferta oferta1;
   private final Oferta oferta2;
   private final double preço;
   private final int quantidade;
   
   public Venda( final Oferta primeiraOferta, final Oferta segundaOferta, final double preço,
      final int quantidade )
   {
      this.oferta1 = primeiraOferta;
      this.oferta2 = segundaOferta;
      this.preço = preço;
      this.quantidade = quantidade;
   }
   
   /**
    * @return the primeira
    */
   public Oferta getOferta1()
   {
      return this.oferta1;
   }
   
   /**
    * @return the segunda
    */
   public Oferta getOferta2()
   {
      return this.oferta2;
   }
   
   public double getPreço()
   {
      return this.preço;
   }
   
   public int getQuantidade()
   {
      return this.quantidade;
   }
   
   public String vendaToString()
   {
      final String açãoEmOferta = "Venda da ação: " + this.getOferta1().getNome()
         + " realizada ao preço de: " + this.getPreço() + " na quantidade de: "
         + this.getQuantidade();
      
      return açãoEmOferta;
   }
}
