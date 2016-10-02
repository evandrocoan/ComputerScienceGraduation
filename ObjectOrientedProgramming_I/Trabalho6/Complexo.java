/*
 * Classe que possibilita tratar um número imaginario como objeto
 */

package Trabalho6;

/**
 * Classe que possibilita tratar um número imaginario como objeto.
 * z = a + bi
 * a --> parte real
 * b --> parte imaginária
 * i --> (-1)^(1/2)
 * Ex:  2 - 3i
 * 4.8 + 3.9i
 * 
 * Nesta classe (além de construtor):
 * a) Retorna a parte real do imaginario representado pelo objeto.
 * b) Retorna a parte imaginária do imaginario representado pelo objeto.
 * c) Retorna o conjugado do imaginario representado pelo objeto. 
 *  (objeto da classe imaginario)
 * d) Método que retorna a soma do imaginario representado com o objeto executor.
 * e) Idem para subtração, multiplicação e divisão.
 * f) Método que retorna o imaginario na forma de String.
 * g) Método que retorne o módulo do número imaginario representado pelo 
 *  objeto. ( (pr)^2 + (pi)^2 )^(1/2)
 * @author Professional
 */
public class Complexo 
{
   /**
    * z = a + bi
    * a --> parte real
    */
   private double real;
   
   /**
    * z = a + bi
    * b --> parte imaginária
    * i --> (-1)^(1/2)
    */
   private double imaginario;
   
   public Complexo()
   {
      real = 0.0;
      imaginario = 0.0;
      
   }
   
   public Complexo( double real, double complexo )
   {
      this.real = real;
      this.imaginario = complexo;
      
   }

   /**
    * Retorna a parte real do imaginario representado pelo objeto
    * @return the real
    */
   public double getReal() 
   {
      return real;
      
   }

   /**
    * Retorna a parte imaginária do imaginario representado pelo objeto.
    * @return the imaginario
    */
   public double getImaginario() 
   {
      return imaginario;
      
   }

   /**
    * @param real the real to set
    */
   public void setReal(double real) 
   {
      this.real = real;
      
   }

   /**
    * @param imaginario the imaginario to set
    */
   public void setImaginario(double imaginario) 
   {
      this.imaginario = imaginario;
      
   }
	
   /**
    * Retorna o conjugado do imaginario representado pelo objeto. 
    *  (objeto da classe imaginario)
    * @return the imaginario
    */
   public Complexo getConjugado() 
   {
      return new Complexo( real, -imaginario );
      
   }
   
   /**
    * Método que retorna a soma do imaginario representado com o objeto executor.
    * @param imaginario
    * @return
    */
   public Complexo soma( Complexo complexo )
   {
      return new Complexo( this.real + complexo.getReal(), 
              this.imaginario + complexo.getImaginario() );
      
   }
   
   /**
    * Método que retorna a diferênça do imaginario representado com o  
    *  objeto executor.
    * @param imaginario
    * @return
    */
   public Complexo diferenca( Complexo complexo )
   {
      return new Complexo( this.real - complexo.getReal(), 
              this.imaginario - complexo.getReal() );
      
   }
   
   /**
    * Método que retorna a divisão do imaginario representado com o 
    *  objeto executor.
    * @param imaginario
    * @return
    */
   public Complexo divisao( Complexo complexo )
   {
      double a = this.real;
      double b = this.imaginario;
      double c = complexo.getReal();
      double c2 = Math.pow( c, 2 );
      double d = complexo.getImaginario();
      double d2 = Math.pow( d, 2 );
      
      return new Complexo( ( a * c + d * b ) / ( c2 + d2 ), 
              ( b * c - a * d ) / ( c2 + d2 ) );
      
   }
   
   /**
    * Método que retorna a multiplicação do imaginario representado com o 
    *  objeto executor.
    * @param imaginario
    * @return
    */
   public Complexo multiplicacao( Complexo complexo )
   {
      double a = this.real;
      double b = this.imaginario;
      double c = complexo.getReal();
      double d = complexo.getImaginario();
      
      return new Complexo( a * c - b * d , a * d + b * c );
      
   }
   
   /**
    * Método que retorna o imaginario na forma de String.
    * @return
    */
   public String paraString()
   {
      return real + " " + imaginario + "i";
      
   }
   
   /**
    * Método que retorne o módulo do número imaginario representado pelo 
    *  objeto. ( (pr)^2 + (pi)^2 )^(1/2)
    * @return
    */
   public double modulo()
   {
      return Math.sqrt( Math.pow( real, 2) + Math.pow( imaginario, 2 ) );
      
   }
   
} // end class Complexo
