/*
 * Considere a classe a seguir:
 * 
 * public class ContaCorrente
 * {
 *    private String correntista;
 *    private int numero;
 *    private double saldo;
 *    private double limite // valor >= 0 que especifica até que valor 
 *    negativo de saldo a conta aceita
 * 
 * ...
 * Objeto: c
 * correntista: José
 * numero: 2327
 * limite: 1000
 * saldo: -800
 * 
 * Coloque nesta classe:
 * 1) Um construtor adequado.
 * 2) Um método que quando executado efetue um depósito na conta 
 *  representado pelo objeto, o deposito somente deve ser efetivado 
 *  se o valor a depositar for positivo. O método deve retornar um 
 *  informação dezendo se o deposito foi efetivado ou não.
 * 3) Método que efetue um saque na conta, o saque somente deve 
 *  ser efetuado se o valor a sacar for <= saldo - limite. O método 
 *  deve retornar uma inforamão dizendo se o saque foi efetuado ou não.
 * 4) Métodos de acesso aos atritutos (gatters setter).
 * 5) Método que efetue, se possível uma transferênciade um 
 *  determinado valor para uma outra conta. O método deve 
 *  retornar uma informação dizendo que a transferência foi 
 *  efetuada. Precisa do valor a transferir e a outra conta. 
 * 
 * Observação: Toda conta deve ser tratada como um objeto.
 */
package Trabalho5;

/**
 *
 * @author Professional
 */
public class ContaCorrente 
{
   /*####################### Variáveis de instância #######################*/
    /**
    * O nome do correntista representado pelo objeto
    */
   private String correntista;
   
    /**
    * O número da conta representado pelo objeto
    */
   private int numero;
   
    /**
    * O saldo da conta representado pelo objeto
    */
   private double saldo;
   
    /**
    * Valor maior ou igual à 0 que especifica até que valor negativo de 
    * saldo a conta aceita
    */
   private double limite;
    
   /*####################### Construtores #######################*/
   /**
    * Um construtor quebra-galho
    */
   public ContaCorrente()
    {
       this.correntista = "Ninguém";
       this.limite = 0;
       this.numero = 0;
       this.saldo = 0;
       
    } // termina o construtor quebra-galho

    /**
    * Um construtor adequado
    * @param correntista o nome do proprietário da conta corrente
    * @param numero o número da conta corrente
    * @param saldo o saldo inicial da conta corrente
    * @param limite o limite inicial da conta corrente
    */
   public ContaCorrente( String correntista, int numero, double saldo, 
            double limite )
   {
      this.correntista = correntista;
      this.limite = limite;
      this.numero = numero;
      this.saldo = saldo;

   } // termina o construtor adequado
   
   /*####################### Getters #######################*/
   /**
    * @return the correntista
    */
   public String getCorrentista()
   {
      return correntista;
      
   } // termina um método de acesso

   /**
    * @return the numero
    */
   public int getNumero() 
   {
      return numero;
      
   } // termina um método de acesso

   /**
    * @return the saldo
    */
   public double getSaldo() 
   {
      return saldo;
      
   } // termina um método de acesso

   /**
    * @return the limite
    */
   public double getLimite() 
   {
      return limite;
      
   } // termina um método de acesso
   
   /*####################### Setters #######################*/
   /**
    * @param correntista the correntista to set
    */
   public void setCorrentista(String correntista) 
   {
      this.correntista = correntista;
      
   } // termina um método de acesso

   /**
    * @param numero the numero to set
    */
   public void setNumero(int numero) 
   {
      this.numero = numero;
      
   } // termina um método de acesso

   /**
    * @param saldo the saldo to set
    */
   public void setSaldo(double saldo) 
   {
      this.saldo = saldo;
      
   } // termina um método de acesso

   /**
    * @param limite the limite to set
    */
   public void setLimite(double limite) 
   {
      this.limite = limite;
      
   } // termina um método de acesso
   
   /*####################### Métodos públicos #######################*/
   /**
   * Um método que quando executado efetua um depósito na conta 
   *  representado pelo objeto, o deposito somente deve ser efetivado 
   *  se o valor a depositar for positivo. O método deve retornar um 
   *  informação dezendo se o deposito foi efetivado ou não.
   * @param valor
   * @return resultado o resultado da operação
   */
   public boolean depositarDinheiro( double valor )
   { 
      if( valor <= 0.0 )
      {
         return false;
         
      }
      
      setSaldo(getSaldo() + valor);
      
      return true;
      
   } // termina método que realiza deposito de dinheiro
   
   /**
   * Método que efetue um saque na conta, o saque somente deve 
   *  ser efetuado se o valor a sacar for menor ou igual à saldo - limite. 
   * O método deve retornar uma informação dizendo se o saque foi efetuado 
   *  ou não.
   * @param valor
   * @return resultado o resultado da operação
   */
   public boolean saqueDinheiro( double valor )
   {
      if( ( ( getSaldo() + getLimite() ) < valor ) || valor == 0.0 )
      {
         return false;

      }
      
      setSaldo(getSaldo() - valor);
      
      return true;
   } // termina método que efetua um saque de dinheiro
   
   /**
   * Método que efetua, se possível uma transferência de um 
   *  determinado valor para uma outra conta. O método deve 
   *  retornar uma informação dizendo que a transferência foi 
   *  efetuada. Precisa do valor a transferir e a outra conta. 
   * @param value o valor a transferir
   * @param conta a qual conta transferir
   * @return resultado o resultado da operação
   */
   public boolean transferenciaDinheiro( double value, ContaCorrente conta )
   {
      if( ( value > ( limite + saldo ) ) || value == 0.0 )
      {
         return false;
         
      }
      
      saldo = saldo - value;
      
      conta.setSaldo( conta.getSaldo() + value );
      
      return true;
      
   } // termina método que efetua uma transferência de dinheiro
   
} // termina a classe pública ContaCorrente
