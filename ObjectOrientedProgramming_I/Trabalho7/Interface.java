/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho7;

import javax.swing.JOptionPane;

/**
 *
 * @author Professional
 */
public class Interface 
{
   public static Apostador pegueApostador( int qual )
   {
      String nome = Interface.pegarNome( qual );
      char sexo = Interface.pegarSexo( nome );
      int idade = Interface.pegarIdade( nome );
      int[] aposta = Interface.pegarAposta( nome );
      
      Apostador apostador = new Apostador( nome, sexo, idade, aposta );
      
      return apostador;
      
   }
   
   private static String pegarNome( int qual )
   {
      String name = " ";
      String nameTrim = " ";
      
      while( nameTrim.length() < 3 )
      {
         name = ( JOptionPane.showInputDialog( 
                 String.format("Insira o nome do %s\u00ba apostador", qual) ) );
         nameTrim = name.trim();
         
         if( nameTrim.length() < 3 )
         {
            JOptionPane.showMessageDialog(null, "Nome inválido inserido!\n"
                    + "Nome tem que ter no mínimo 3 caracteres "
                    + "não brancos." );
            
         }
         
      }
      
      return name;
      
   }
   
   private static char pegarSexo( String qual )
   {
      char sexo = ' ';
      boolean isBoolean = true;
      
      // pega o sexo da pessoa
      while( isBoolean || ( sexo != 'F' && sexo != 'M' ) )
      {
         isBoolean = true;
         
         try
         {
            sexo = ( JOptionPane.showInputDialog( 
                    String.format("Insira a o sexo do apostador %s"
                    + " (F ou M): ", qual ) ) ).charAt(0);
            isBoolean = false;
            
            if( sexo != 'F' && sexo != 'M' )
            {
               JOptionPane.showMessageDialog(null, "Valor inválido inserido!"
                       );
               
            }
            
         } catch ( NumberFormatException instanceError )
         {
            JOptionPane.showMessageDialog(null, "Valor inválido inserido!\n"
                    + instanceError.getMessage() );

         }
         
      } // end input block
      
      return sexo;
      
   }
   
   private static int pegarIdade( String qual )
   {
      int idade = 0;
      boolean isBoolean = true;

      // pega a idade da pessoa
      while( isBoolean || idade < 18 || idade > 120 )
      {
         isBoolean = true;
         
         try
         {
            idade = Integer.parseInt( JOptionPane.showInputDialog( 
                    String.format("Insira a idade do apostador %s :", qual) ) );
            isBoolean = false;
            
            if( idade < 18 || idade > 120 )
            {
               JOptionPane.showMessageDialog(null, "Idade inválida inserida!"
                       + "\nPara se poder apostar tem que ter entre "
                       + "18 e 120 anos."
                    );
               
            }
            
         } catch ( NumberFormatException instanceError )
         {
            JOptionPane.showMessageDialog(null, "Valor inválido inserido!\n"
                    + instanceError.getMessage() );
            
         }
         
      } // end input block
      
      return idade;
      
   }
   
   private static int[] pegarAposta( String qual )
   {
      int tam = Interface.pegarApostaTam( qual );
      int[] aposta = new int[ tam ];
      
      for( int i = 0; i < tam; i++ )
      {
         boolean sentinela = false;
         
         while( !sentinela )
         {
            aposta[i] = Interface.pegarApostaNum( qual, i );
            
            sentinela = true;
            
            for( int k = i - 1; k >= 0; k-- )
            {
               if( aposta[i] == aposta[k] )
               {
                  sentinela = false;
                  Interface.mostrarMensagem( "Insíra um número diferente"
                          + " dos anteriores!" );
                  break;
                  
               }
               
            }
            
         }
         
      }
      
      return aposta;
      
   }
   
   private static int pegarApostaNum( String qual, int i )
   {
      int aposta = 0;
      
      while( aposta < 1 || aposta > 60 )
      {
         aposta = Interface.pegueInteiroPositivo( String.format( 
              "%s, Insira o %s\u00ba d\u00edgito da aposta.", qual, i + 1) );
         
         if( aposta < 1 || aposta > 60 )
         {
            Interface.mostrarMensagem( "Valor inválido inserido!"
                    + "\n" + "Insira um n\u00famero de 1 \u00e0 60." );
            
         }
         
      }
      
      return aposta;

   }
   
   private static int pegarApostaTam( String qual )
   {
      int tam = 0;
      
      while( tam < 6 || tam > 10 )
      {
         tam = Interface.pegueInteiroPositivo( String.format( 
                 "%s, escolha quantos n\u00fameros apostar"
                 + "\n6 n\u00fameros - R$ 1.00\n7 n\u00fameros - R$ 7.00"
                 + "\n8 n\u00fameros - R$ 28.00\n9 n\u00fameros - R$ 168.00"
                 + "\n10 n\u00fameros - R$ 1260.00", qual ) );
         
         if( tam < 6 || tam > 10 )
         {
            Interface.mostrarMensagem( "Quantidade de números inválida!" );
            
         }
         
      }
      
      return tam;
      
   }
   
   public static int pegueInteiroPositivo( String mensagem )
   {
      int inteiroPositivo = 1; // menor inteiro positivo
      boolean isBoolean = true;
      
      while( isBoolean || inteiroPositivo < 1 )
      {
         isBoolean = true;
         
         try
         {
            inteiroPositivo = Integer.parseInt( JOptionPane.showInputDialog( 
                    mensagem ) );
            isBoolean = false;

         } catch ( NumberFormatException instanceError )
         {
            JOptionPane.showMessageDialog(null, "Valor inválido inserido!\n"
                    + instanceError.getMessage() );

         }
         
      } // end input block
      
      return inteiroPositivo;
      
   }
   
   public static void mostrarMensagem( String msg )
   {
      JOptionPane.showMessageDialog(null, msg);
      
   }
   
} // end class Interface
