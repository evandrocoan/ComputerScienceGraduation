/* Escreva um programa que analize os apostadores da mega-sena.
 * O programa deve simular o sorteio da mega-sena, isto é, 
 *  sortear 6 números inteiros todos no intervalo 1 à 60 (incluindo 1 e 60), 
 *  e todos diferentes.
 * Cada apostador pode apostar de 6 à até dez números 
 *  ( todos no intervalo de 1 à 60 e todos diferentes) considere que, 
 *  se apostar 6 números o apostador pagará um real, 
 *  se apostar 7 números pagará 7 reais, 
 *  se apostar 8 números pagará 28 reais, 
 *  se apostar 9 números pagará 168 reais, e 
 *  se apostar 10 números pagará 1260 reais.
 * O programa deve mostrar o nome e o total de pontos de cada apostador.
 * O programa deve também mostrar o total arrecadado, isto é, o total 
 *  pago pelos apostadores.
 * 
 * Math.random()
 *  para sortear um double entre 0 e 1, podendo sortear 0, mas nunca 1.
 * E 
 *  n = (int) ( Math.random() * 60 );
 * Cuidar para o intervalo ser de 1 à 60 e para o método
 * 
 * Solicitar os dados do apostador, o tipo de aposta 
 *  ( de 6 à 10 números e 
 *  na faixa de 1 à 60 e 
 *  não podendo ter repetição ao escolher números )
 * 
 * 
 * Classes: 
 *  Apostador
 *  Pessoa
 * (Pode-se sortear direto no main)
 *  Principal
 *  Interface
 */

package Trabalho7;

/**
 *
 * @author Professional
 */
public class Principal 
{
	// main method begins execution Java application
	public static void main( String[] args )
	{
		int apostadoresNumero = Interface.pegueInteiroPositivo( "Insíra quantos "
              + "apostadores participarão do sorteio da Mesa-Sena: " );
      
      Apostador[] apostadores = new Apostador[apostadoresNumero];
      
      for( int i = 0; i < apostadoresNumero; i++ )
      {
         apostadores[i] = Interface.pegueApostador( i + 1 );
         
      }
      
      Interface.mostrarMensagem( Principal.resultadoSorteio( apostadores ) );
      
	} // end main method
   
   private static String resultadoSorteio( Apostador[] apostadores )
   {
      int sorteio[] = Principal.sortear();
      String resultado = String.format("Os resultados do sorteio %s "
              + "foram:\n", Principal.paraString( sorteio ) );
      
      for( int i = 0; i < apostadores.length; i++ )
      {
         int pontos = 0;
         
         for( int k = 0; k < apostadores[i].getAposta().length; k++ )
         {
            
            for( int j = 0; j < 6; j++ )
            {
               if( sorteio[j] == apostadores[i].getAposta()[k] )
               {
                  pontos++;
                  
               }
               
            }

         }
         
         resultado += String.format( "\n%s com %d pontos (%s )", 
                 apostadores[i].getNome(), pontos, 
                 apostadores[i].paraString() ) ;
         
      }
      
      resultado += String.format( "\n\nE o total arrecadado foi: R$ %.2f", 
              Principal.obterPremio(apostadores) );
      
      return resultado;
      
   }
	
   private static int[] sortear()
   {
      int[] array = new int[ 6 ];
      
      for( int i = 0; i < 6; i++ )
      {
         array[i] = Apostador.fornecaSorteado1_60();
         
      }
      
      Apostador.corrigirAposta( array );
      
      return array;
      
   }
   
   private static double obterPremio( Apostador[] apostadores )
   {
      double premio = 0;
      
      for( int i = 0; apostadores.length > i; i++ )
      {
         switch( apostadores[i].getAposta().length )
         {
            case 6: premio += 1; break;
            case 7: premio += 7; break;
            case 8: premio += 28; break;
            case 9: premio += 168; break;
            default: premio += 1260;
               
         }
         
      }
      
      return premio;
      
   }
   
   public static String paraString(int[] array)
   {
      String string = "";
      
      for( int i = 0; i < array.length; i++ )
      {
         string += String.format(" %s", array[i]);
         
      }
      
      return string;
      
   }
   
} // end class Principal
