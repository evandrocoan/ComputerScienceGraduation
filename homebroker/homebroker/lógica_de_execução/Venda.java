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
}
