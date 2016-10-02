/*
 * Faz a comunicação entre o usuário e programa
 */

package Trabalho6;

import javax.swing.JOptionPane;

/**
 *
 * @author Professional
 */
public class Interface 
{
   public Complexo solicitarComplexo()
   {
      return new Complexo( 
              this.obterQuantia( "Insira a parte real do número complexo: ", 
              "Valor inválido inserido!\n Insira números."), 
              this.obterQuantia( 
              "Insira a parte imaginária do número complexo: ", 
              "Valor inválido inserido!\n Insira números." ) );
      
   }
   
   /**
    * Monstra uma mensagem na tela
    * @param men a mensagem para exibir
    */
   public void monstrarMensagem( String men )
   {
      JOptionPane.showMessageDialog(null, men);
      
   }
   
   /**
    * Solicita um número double ao usuário
    * @return numero o número solicidado
    * @param solicitacao a solicitação ao usuário
    * @param valorInvalido o mensagem para valor inválido inserido
    */
   private double obterQuantia( String solicitacao, String valorInvalido )
   {
      double quantia = 0.0;
      String quantiaText = " ";
      boolean isBoolean = true;
      
      // bloco que obtém a quantia
      while( ( quantia < 0.0 ) || isBoolean )
      {
         isBoolean = true;
         
         try
         {
            quantiaText = JOptionPane.showInputDialog( solicitacao );
            quantia = Double.parseDouble( quantiaText );
            isBoolean = false;
            
         } catch( NumberFormatException instanceError )
         {
            // encerra o programa caso seja clicado no botão 'Cancelar'
            if( quantiaText == null )
            {
               Integer.parseInt( quantiaText ); 
               
            }
            
            JOptionPane.showMessageDialog(null, valorInvalido + "\n" 
                    + instanceError.getMessage() );
            
         }
         
      } // termina bloco que obtém a quantia
      
      return quantia;
      
   }
   
} // end class Interface
