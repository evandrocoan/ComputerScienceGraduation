/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho7;

/**
 *
 * @author Professional
 */
public class Apostador extends Pessoa
{
   private int[] aposta;
   
   public Apostador( String nome, char sexo, int idade, int[] aposta )
   {
      super( nome, sexo, idade );
      
      if( aposta == null )
      {
         aposta = this.sortearAposta();
         
      }
      
      if( aposta.length < 6 || aposta.length > 10 )
      {
         aposta = this.sortearAposta();
         
      } else
      {
         Apostador.corrigirAposta( aposta );
         
      }
      
      this.aposta = aposta;
      
   }

   /**
    * @return the aposta
    */
   public int[] getAposta() 
   {
      return aposta;
      
   }

   /**
    * @param aposta the aposta to set
    */
   public void setAposta( int[] aposta ) 
   {
      if( aposta == null )
      {
         aposta = this.sortearAposta();
         
      }
      
      if( aposta.length < 6 || aposta.length > 10 )
      {
         aposta = this.sortearAposta();
         
      } else
      {
         Apostador.corrigirAposta( aposta );
         
      }
      
      this.aposta = aposta;
      
   }
   
   private int[] sortearAposta()
   {
      int sort = Apostador.fornecaSorteado6_10();
      int[] array = new int[ sort ];
      
      for( int i = 0; i < sort; i++ )
      {
         array[i] = Apostador.fornecaSorteado1_60();

      }
      
      Apostador.corrigirAposta( array );
              
      return array;
      
   }
   
   public static void corrigirAposta( int[] aposta )
   {
      int i2 = 0;

      do
      {
         double anterior = aposta[ i2++ ];

         for( int i = i2; i < aposta.length; i++ )
         {
            if( anterior == aposta[i] )
            {
               aposta[i] = Apostador.fornecaSorteado1_60();
               i2 = 0;
               break;

            }

         }
         
      } while( i2 < aposta.length );
      
   }
   
   public static int fornecaSorteado1_60()
   {
      return (int) ( Math.random() * 60 ) + 1;
      
   }
   
   public static int fornecaSorteado6_10()
   {
      return (int) ( Math.random() * 5 ) + 6;
      
   }
   
   @Override
   public String paraString()
   {
      String string = "";
      
      for( int i = 0; i < aposta.length; i++ )
      {
         string += String.format(" %s", aposta[i]);
         
      }
      
      return string;
      
   }
   
} // end class Apostador
