/* 
 * Faz a comunicação entre o usuário e o computador
 */

package Trabalho4;

import javax.swing.JOptionPane;

/**
 * Faz a comunicação entre o usuário e o computador
 * @author Evandro  Coan
 */

public class Interface 
{
   /**
    * Prompt the user to input an integer from 0 to 90 degrees.
    * @return angleDouble the angle to compute
    */
   public double inputAngle()
   {
      // informa à repetição se o valor é numérico
      boolean inputSentinel = true;
      
      boolean isInteger = false; // informa à repetição se o valor é inteiro
      String angle = ""; // ângulo inserido pelo usuário como texto
      double angleDouble = 0.0; // ângulo insido pelo usuário como número
      
      // repetição que obtém os valores corretos do usuário
      while( ( inputSentinel || !isInteger || angleDouble < 0 || 
              angleDouble > 90 ) & null != angle  )
      {
         // re-ativa a verificação quando um valor válido fora da faixa de 
         // valores (0.0 ... 90.0) é inserido
         inputSentinel = true;
         
         // ativa a mensagem de erro reserva para valores fora da faixa 
         // 0...90 e não inteiros
         boolean wasShow = true;
         
         // tenta obter valores numéricos do usuário
         try
         {
            angle = JOptionPane.showInputDialog( 
                          "Insira um inteiro entre 0 e 90 graus." );
            angleDouble = Double.parseDouble( angle );
            
            inputSentinel = false;

         } catch( NumberFormatException instanceError )
         {
            // menssagem de erro para valores não numéricos
            JOptionPane.showMessageDialog( null, "Valor inválido inserido "
                    + "para: " + angle + "\n" + instanceError.getMessage() );
            
            // informa à mensagem de erro reserva que ela não precisa ser 
            // mais exibida
            wasShow = false;
            
         }
         
         // verifica se o ângulo é um inteiro
         if( angleDouble % 1 != 0 )
         {
            isInteger = false; // informa à repetição que o valor não é inteiro
            
         } else
         {
            isInteger = true; // informa à repetição que o valor é inteiro
            
         } // termina verificação de valores inteiros
         
         // mensagem de erro reserva para valores fora da faixa 0...90 e 
         // não inteiros
         if( ( !isInteger || angleDouble < 0 || angleDouble > 90 ) 
                 & wasShow ) 
         {
            JOptionPane.showMessageDialog( null, "Valor inválido inserido "
                    + "para: " + angle );
            
         } // termina mensagem de erro para valores numéricos inválidos
         
      } // termina a repecição que obtém os dados do usuário
      
      return angleDouble; // retorna o ângulo para o método invocador
      
   } // termina método que obtém o ângulo do usuário
   
   /**
    * Obtém do usuário a precisão do cálculo para seno e cosseno entre 
    * 1 e 10
    * @return precisao a precisão para cálculo de seno e cosseno
    */
   public int inputPrecision()
   {
      // informa à repetição se o valor é numérico
      boolean inputSentinel = true;
      
      String precision = ""; // precisão inserido pelo usuário como texto
      
      // precisão insido pelo usuário como número
      int precisionInteger = 0; 
      
      // repetição que obtém os valores corretos do usuário
      while( ( inputSentinel || precisionInteger < 1 || 
              precisionInteger > 10 ) & null != precision )
      {
         // re-ativa a verificação quando um valor válido fora da faixa de 
         // valores (0.0 ... 90.0) é inserido
         inputSentinel = true;
         
         // ativa a mensagem de erro reserva para valores fora da faixa 
         // 0...90 e não inteiros
         boolean wasShow = true;
         
         // tenta obter valores numéricos do usuário
         try
         {
            precision = JOptionPane.showInputDialog( 
                          "Insira a precisão dos cálculos de seno e "
                    + "cosseno entre 1 e 10." );
            precisionInteger = Integer.parseInt( precision );
            
            inputSentinel = false;

         } catch( NumberFormatException instanceError )
         {
            // menssagem de erro para valores não numéricos
            JOptionPane.showMessageDialog( null, "Valor inválido inserido "
                    + "para: " + precision + "\n" 
                    + instanceError.getMessage() );
            
            // informa à mensagem de erro reserva que ela não precisa ser 
            // mais exibida
            wasShow = false;
            
         }
         
         // mensagem de erro para valores fora da faixa 1...10
         if( ( precisionInteger < 1 || precisionInteger > 10 ) 
                 & wasShow )
         {
            JOptionPane.showMessageDialog( null, "Valor inválido inserido "
                    + "para: " + precision );
            
         } // termina mensagem de erro para valores numéricos inválidos
         
      } // termina a repecição que obtém os dados do usuário
      
      return precisionInteger; // retorna o ângulo para o método invocador
      
   } // termina método que obtém a precisão de calculo do usuário
   
   /**
    * Mostra o conteúdo de seu parâmetro string na tela
    * @param string
    */
   public void showMessage( String string )
   {
      // mostra o conteúdo da variável string na tela
      JOptionPane.showMessageDialog(null, string);
      
   } // termina método que mostra menssagem
   
} // termina a classe Interface
