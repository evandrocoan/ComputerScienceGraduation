/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_dados;

/**
 * Representa um ação do mercado de valores. Cada objeto desta classe representa
 * uma ação com um nome e valor de mercado. Cada objeto contém também o número
 * de ações que ele representa.
 * 
 * @author Professional
 */
public class Ação
{
   private double preço;
   private int quantidade;
   private String nome;
   
   /**
    * Construtor que cria uma ação completa.
    * 
    * @param preco o preço da ação
    * @param quantidade a quantidade de ações que este objeto representa
    * @param nome o nome da ação que este objeto representa
    */
   public Ação( final double preco, final int quantidade, final String nome )
   {
      this.preço = preco;
      this.quantidade = quantidade;
      this.nome = nome;
   }
   
   /**
    * Retorna um String com o nome dessa ação.
    * 
    * @return nome um String com o nome da ação
    */
   public String getNome()
   {
      return this.nome;
   }
   
   /**
    * Retorna o preço da ação.
    * 
    * @return preco o preço da ação
    */
   public double getPreço()
   {
      return this.preço;
   }
   
   /**
    * @return the quantidade
    */
   public int getQuantidade()
   {
      return this.quantidade;
   }
   
   /**
    * @param nome the nome to set
    */
   public void setNome( final String nome )
   {
      this.nome = nome;
   }
   
   /**
    * Define um preço para a ação.
    * 
    * @param preco o novo preço da ação
    */
   public void setPrice( final double preco )
   {
      this.preço = preco;
   }
   
   /**
    * Refine a quantidade de ações.
    * 
    * @param quantidade a quantidade de ações que este objeto representa
    * @return true caso tenha sucesso, false caso contrário
    */
   public boolean setQuantidade( final int quantidade )
   {
      if( this.quantidade >= Math.abs( quantidade ) )
      {
         this.quantidade += quantidade;
         return true;
      }
      return false;
   }
}
