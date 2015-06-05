/**
 *  
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
   
   public Venda( final Oferta primeiraOferta, final Oferta segundaOferta )
   {
      this.oferta1 = primeiraOferta;
      this.oferta2 = segundaOferta;
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
   
   public String vendaToString()
   {
      final String açãoEmOferta =
         "Venda da ação: " + this.getOferta1().getAção().getNome() + " realizada de: "
            + this.getOferta1().getConta().getNome() + " para: "
            + this.getOferta2().getConta().getNome() + " ao preço de: "
            + this.getOferta1().getAção().getPreço() + " na quantidade de: "
            + this.getOferta1().getAção().getQuantidade();
      
      return açãoEmOferta;
   }
}
