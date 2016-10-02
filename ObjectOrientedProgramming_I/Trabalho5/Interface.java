/*
 * Faz a comunicação entre o usuário e programa
 */

package Trabalho5;

import javax.swing.JOptionPane;

/**
 *
 * @author Professional
 */
public class Interface 
{
   /*################### Métodos Públicos ###################*/
   /**
    * Solicita os dados de uma conta corrente
    * @return conta a conta cadastrada
    */
   public ContaCorrente cadastrarContaCorrente()
   {
      String name = " ";
      String nameTrim = " ";
      int numero = -1;
      double saldo = -1.0;
      double limite = -1.0;
      
      // bloco que pega o nome do correntista
      while( nameTrim.length() < 3 )
      {
         name = ( JOptionPane.showInputDialog( 
                 "Insira o nome do correntista" ) );
         nameTrim = name.trim();
         
         if( nameTrim.length() < 3 )
         {
            JOptionPane.showMessageDialog(null, "Nome inválido inserido!\n"
                    + "Nome tem que ter no mínimo 3 caracteres "
                    + "não brancos." );
            
         }
         
      } // termina o bloco que pega o nome do correntista
      
      numero = this.obterNumero( "Insira o número da conta corrente: ", 
              "Valor inválido inserido!", "Quantia inválida inserida "
                    + "pois é negativa!" );
      
      saldo = this.obterQuantia( "Insiro o saldo inicial da conta: ", 
              "Valor inválido inserido!", "Quantia inválida inserida "
                    + "pois é negativa!" );
      
      limite = this.obterQuantia( "Insira o limite da conta corrente: ", 
              "Valor inválido inserido!", "Quantia inválida inserida "
                    + "pois é negativa!");
      
      ContaCorrente conta = new ContaCorrente( name, numero, saldo, limite);
      
      return conta;
      
   } // termina método que cadastra a conta corrente
   
   /**
    * Mostra o conteúdo de um objeto da classe String
    * @param mensagem a mensagem a ser exibida
    */
   public void monstrarMensagem( String mensagem )
   {
      JOptionPane.showMessageDialog(null, mensagem);
      
   } // termina método que mostra o conteúdo de um objeto da classe String
   
   /**
    * Obtém a instrução do usuário ao programa
    * @return opcao a opção do usuário
    */
   public int obterOpcao()
   {
      int opcao = 0;
      
      // bloco que obtém o comando do usuário
      while( !( opcao == 1 || opcao == 2 || 
              opcao == 3 || opcao == 4 || 
              opcao == 5 ) )
      {
         opcao = this.obterNumero( "Menu de opções: \n" 
                  + "1 - Efetuar depósito\n" 
                  + "2 - Efetuar saque\n" 
                  + "3 - Consultar saldo\n" 
                  + "4 - Efetuar transferência\n" 
                  + "5 - Finalizar\n"
                  + "Insira qual o próxima instrução: ", 
                 
                  "Valor inválido inserido!", 
                  
                  "Valor inválido inserido!\nO menu de opções é: \n" 
                  + "1 - Efetuar depósito\n" 
                  + "2 - Efetuar saque\n" 
                  + "3 - Consultar saldo\n" 
                  + "4 - Efetuar transferência\n" 
                  + "5 - Finalizar" );
         
         if( opcao == 0 )
         {
            return 5;
            
         }
         
         if( !( opcao == 1 || opcao == 2 || 
              opcao == 3 || opcao == 4 || 
              opcao == 5 ) && ( opcao > 5 ) )
         {
            JOptionPane.showMessageDialog(null, "Valor inválido inserido!\n" 
                    + "O menu de opções é:\n" 
                    + "1 - Efetuar depósito\n" 
                    + "2 - Efetuar saque\n" 
                    + "3 - Consultar saldo\n" 
                    + "4 - Efetuar transferência\n" 
                    + "5 - Finalizar");
            
         }
         
      } // termina bloco que obtém o comando do usuário
      
      return opcao;
      
   } // termina método que obtém a instrução do usuário
	
   /**
    * Efetua um saque na conta corrente
    * @param conta a conta a receber a transferência
    * @return resultado o resultado da operação
    */
   public String sacarDinheiro( ContaCorrente conta )
   {
      double quantia = this.obterQuantia( "Insira a quantia a ser sacada: ", 
              "Valor inválido inserido!", "Quantia inválida inserida!" 
              + "\nPara sacar quantias negativas faça depósitos " 
              + "positivos.");
      
      // realiza o saque
      if( conta.saqueDinheiro( quantia ) )
      {
         return "A operação foi realizada com sucesso.";
         
      }
      
      return "A operação foi cancelada.";
      
   } // termina método que efetua saque
   
   /**
    * Efetua um depósito na conta corrente
    * @param conta
    * @return mensagem a mensagem do estado da operação
    */
   public String depositarDinheiro( ContaCorrente conta )
   {
      double quantia = this.obterQuantia( "Insira a quantia a ser "
              + "depositada: ", "Valor inválido inserido!", 
              "Quantia inválido inserida!\nPara depositar uma "
              + "quantia negativa, faça um saque com quantia positiva.");
      
      // realiza o deposito
      if( conta.depositarDinheiro( quantia ) )
      {
         return "A operação foi realizada com sucesso.";
         
      }
      
      return "A operação foi cancelada.";
      
   } // termina método que efetua depósito
   
   /**
    * Efetua uma transferência de dinheiro entre contas
    * @param contaOrigem a conta de onde sairá o dinheiro
    * @param contaDestino a conta onde chegará o dinheiro
    * @return resultado o resultado da transferência
    */
   public String transferenciaDinheiro( ContaCorrente contaOrigem, 
           ContaCorrente contaDestino )
   {
      double quantia = this.obterQuantia( "Insira a quantia a ser "
              + "transferida: ", "Valor inválido inderido!", "Quantia "
              + "inválida inserida!\nPara transferir quantias negativas "
              + "para a outra conta, faça transferências positivas "
              + "daquela conta para esta." );
      
      if( contaOrigem.transferenciaDinheiro( quantia, contaDestino ) )
      {
         return "A transferência foi realizada com sucesso.";
         
      }
      
      return "A transferência foi cancelada.";
      
   } // termina o método que efetua uma transferência de dinheiro entre contas
   
	/*################### Métodos Private ###################*/
   /**
    * Solicita quantia ao usuário
    * @return obterQuantia a obterQuantia solicidada
    * @param solicitacao a solicitação ao usuário
    * @param valorInvalido o mensagem para valor inválido inserido
    * @param quantiaInvalida a mensagem para quantia inválida inserida
    */
   private double obterQuantia( String solicitacao, String valorInvalido, 
           String quantiaInvalida )
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
            if( quantiaText == null )
            {
               return 0.0;
               
            }
            
            JOptionPane.showMessageDialog(null, valorInvalido + "\n" 
                    + instanceError.getMessage() );
            
         }
         
         if( ( quantia < 0.0 ) && !isBoolean )
         {
            JOptionPane.showMessageDialog(null, quantiaInvalida );
            
         }
         
      } // termina bloco que obtém a quantia
      
      return quantia;
      
   }
   
   /**
    * Solicita número inteiro positivo ao usuário
    * @return numero o número solicidado
    * @param solicitacao a solicitação ao usuário
    * @param valorInvalido a mensagem para valor inválido inserido
    * @param numeroInvalido a mensagem para número inválido inserido
    */
   private int obterNumero( String solicitacao, String valorInvalido, 
           String numeroInvalido )
   {
      int numero = 0;
      String numeroText = " ";
      boolean isBoolean = true;
      
      // bloco que obtém o número
      while( ( numero < 0 ) || isBoolean )
      {
         isBoolean = true;
         
         try
         {
            numeroText = JOptionPane.showInputDialog( solicitacao );
            numero = Integer.parseInt( numeroText );
            isBoolean = false;
            
         } catch( NumberFormatException instanceError )
         {
            if( numeroText == null )
            {
               return 0;
               
            }
            
            JOptionPane.showMessageDialog(null, valorInvalido + "\n" 
                    + instanceError.getMessage() );
            
         }
         
         if( ( numero < 0 ) && !isBoolean )
         {
            JOptionPane.showMessageDialog(null, numeroInvalido );
            
         }
         
      } // termina bloco que obtém o número
      
      return numero;
      
   } // termina o método que obtém o número inteiro positivo
   
} // end class Interface
