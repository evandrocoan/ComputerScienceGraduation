/**
 * 
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 * 
 * @author Professional
 */
public final class JanelaDeCadastro extends JFrame
{
   private static JanelaDeCadastro instância;
   
   private final MotorDoHomebroker motor;
   
   private JanelaDeCadastro()
   {
      this.motor = MotorDoHomebroker.getInstância();
   }
   
   /**
    * @return instância uma instância da janela de login.
    */
   public static JanelaDeCadastro getInstância()
   {
      synchronized( JanelaDeCadastro.class )
      {
         if( JanelaDeCadastro.instância == null )
         {
            JanelaDeCadastro.instância = new JanelaDeCadastro();
         }
      }
      return JanelaDeCadastro.instância;
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarBloqueio()
   {
      if( !this.motor.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há "
                  + "nenhuma conta carregada no sistema!" );
         return;
      }
      if( !this.motor.isAdministradora() )
      {
         JOptionPane.showMessageDialog( null, "Acesso negado! "
                  + "Você precisa ter privilégio de administrador." );
         return;
      }
      this.solicitarConta();
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarCadastro()
   {
      if( !this.motor.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há "
                  + "nenhuma conta carregada no sistema!" );
         return;
      }
      boolean sucesso = false;
      
      while( !sucesso )
      {
         final String nome = this.getNome();
         if( nome == null )
         {
            return;
         }
         final String senha = this.getSenha();
         if( senha == null )
         {
            return;
         }
         final double saldo = this.getSaldo();
         if( saldo == -1 )
         {
            return;
         }
         final int cpf = this.getCPF();
         if( cpf == 0 )
         {
            return;
         }
         sucesso = this.motor.adicionarConta( saldo, cpf, nome, senha );
      }
      
   }
   
   /**
    * 
    */
   public void excluirConta()
   {
      /* TODO @formatter:off
       * 
       * 
       */ //@formatter:on
      
   }
   
   private int getCPF()
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int cpf = 0;
      
      while( !sucesso )
      {
         final String input =
                  JOptionPane.showInputDialog( ( nÉsimaVez? "CPF inválido!\n\n"
                           : "" ) + "Insira um CPF:" );
         if( input == null )
         {
            return 0;
         }
         try
         {
            cpf = (int) Double.parseDouble( input );
            sucesso = String.valueOf( cpf ).length() == 9;
         } catch( final Exception e )
         {
            e.printStackTrace();
         }
         nÉsimaVez = true;
      }
      return cpf;
   }
   
   private String getNome()
   {
      String nome = null;
      
      nome =
               JOptionPane
                        .showInputDialog( "Insira o nome novo do acionista: \n" );
      
      return nome;
   }
   
   private double getSaldo()
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int input = 0;
      
      while( !sucesso )
      {
         final String inputString =
                  JOptionPane.showInputDialog( ( nÉsimaVez
                           ? "Saldo inválido!\n\n" : "" )
                           + "Insira o saldo do novo acionista:" );
         if( inputString == null )
         {
            return -1;
         }
         try
         {
            input = (int) Double.parseDouble( inputString );
            sucesso = true;
         } catch( final Exception e )
         {
            e.printStackTrace();
         }
         nÉsimaVez = true;
      }
      return input;
   }
   
   private String getSenha()
   {
      String nome = null;
      
      nome =
               JOptionPane
                        .showInputDialog( "Insira a senha do novo acionista: \n" );
      
      return nome;
   }
   
   private void solicitarConta()
   {
      String nome = null;
      boolean inputError = true;
      do
      {
         nome =
                  JOptionPane.showInputDialog( ( inputError? ""
                           : "Usuário inválido!\n\n" )
                           + this.motor.contasTesteToString()
                           + "\n\nInsira qual conta será bloqueada: " );
         
         if( nome == null )
         {
            return;
         }
         inputError = this.motor.bloquearConta( nome );
         
      } while( !inputError );
      
      JOptionPane.showMessageDialog( null, "Bloqueio realizado com sucesso!" );
   }
}
