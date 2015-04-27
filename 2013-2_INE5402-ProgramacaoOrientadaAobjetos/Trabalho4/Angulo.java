/*
 * Representar um ângulo e calcular seu seno e cosseno
 */

package Trabalho4;

/**
 * Representar um ângulo e calcular seu seno e cosseno
 * @author Evandro  Coan
 */
public class Angulo 
{
   /**
    * variável que representa o objeto da classe
    */
   private double angulo;
   
   /**
    * Construtor que inicializa a instância para o valor 0.0
    */
   public Angulo()
   {
      this.angulo = 0.0;
      
   } // termina o construtor da classe
   
   /**
    * Construtor que arredonda o ângulo recebido para o inteiro mais próximo
    * e senão para cima. E seguinte, caso o ângulo seja maior que 90 graus 
    * e menor que zero, obtém seu valor entre zero e 90 graus.
    * @param angulo
    */
   public Angulo( double angulo )
   {
      // arredonda para o inteiro mais próximo, senão arredonda para cima
      this.angulo = Math.round( ( (float) angulo ) );
      
      // obtém o valor do ângulo entre 0 e 90 para cálculo
      if( this.angulo < 0 || this.angulo > 90 )
      {
         this.angulo = this.angulo % 90;
      }
      
      if( this.angulo < 0 )
      {
         this.angulo = this.angulo + 90;
         
      }
      
   } // termina o construtor da classe

   /**
    * Converte um ângulo em graus para radianos.
    * @return ângulo em radiano
    */
   private double fornecaRadianos()
   {
      return ( angulo * Math.PI ) / 180;

   } // termina o método que converte graus para radianos
   
   /**
    * Calculo o fatorial de um número.
    * @param numero
    * @return o fatorial de numero
    */
   private double getFactorial( int numero )
   {
      double factorial = 1;
      
      while( numero > 1 )
      {
         factorial = numero * factorial;
         
         numero--;
         
      }
      
      return factorial;
      
   } // termina o método que cálcula o fatorial
   
   /**
    * Calcula o cosseno da variável de instância <b>angulo</b> pela série: 
    * sin(x) = x - x^3/3! + x^5/5! - x^7/7! ... (-1)^(n+1)x^n/n!
    * @param precisao do cálculo
    * @return cosseno do ângulo
    */
   public double cos( int precisao )
   {
      double cos = 0.0;
      int passos = 0;
      int sinal = 0;
      double radianos = fornecaRadianos();
      
      while( precisao > sinal )
      {
         cos = cos + ( Math.pow( -1, sinal++ ) * 
                 Math.pow( radianos, passos ) 
                      ) / getFactorial( passos );
         
         passos = passos + 2;
         
      }
      
      return cos;
      
   } // termina o método que calcula o cosseno
   
   /**
    * Calcula o seno da variável de instância <b>angulo</b> pela série: 
    * sin(x) = x - x^3/3! + x^5/5! - x^7/7! ... (-1)^(n+1)x^n/n!
    * @param precisao do cálculo
    * @return seno do ângulo
    */
   public double sin( int precisao )
   {
      double sin = 0;
      int passos = 1;
      int sinal = 0;
      double radianos = fornecaRadianos();
      
      while( precisao > sinal )
      {
         sin = sin + ( Math.pow( -1, sinal++ ) * 
                 Math.pow( radianos, passos ) 
                      ) / getFactorial( passos );
         
         passos = passos + 2;
         
      }
      
      return sin;
      
   } // termina o método que calcula o seno
   
} // end class Angulo
