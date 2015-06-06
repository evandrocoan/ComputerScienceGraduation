/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 *
 * @author Professional
 */
public final class JanelaDeCadastro extends JFrame
{
   private static JanelaDeCadastro INSTÂNCIA;
   
   private final Fachada fachada;
   
   private JanelaDeCadastro()
   {
      this.fachada = Fachada.getInstância();
   }
   
   /**
    * @return instância uma instância da janela de login.
    */
   public static JanelaDeCadastro getInstância()
   {
      synchronized( JanelaDeCadastro.class )
      {
         if( JanelaDeCadastro.INSTÂNCIA == null )
         {
            JanelaDeCadastro.INSTÂNCIA = new JanelaDeCadastro();
         }
      }
      return JanelaDeCadastro.INSTÂNCIA;
   }
   
   /**
    * Efetua o bloqueio de uma conta.
    */
   public void efetuarBloqueio()
   {
      if( !this.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há " + "nenhuma conta carregada no sistema!" );
         return;
      }
      if( !this.fachada.isAdministradora() )
      {
         JOptionPane.showMessageDialog( null, "Acesso negado! "
            + "Você precisa ter privilégio de administrador." );
         return;
      }
      final String conta = this.solicitarConta( "\n\nInsira qual conta será bloqueada: " );
      
      if( conta != null )
      {
         JOptionPane.showMessageDialog( null, "Bloqueio realizado com sucesso!" );
         this.fachada.bloquearConta( conta );
      }
   }
   
   /**
    * Efetua o cadastro de uma nova conta no Sistema.
    */
   public void efetuarCadastro()
   {
      if( !this.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há " + "nenhuma conta carregada no sistema!" );
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
         sucesso = this.fachada.adicionarConta( saldo, cpf, nome, senha );
      }
      JOptionPane.showMessageDialog( null, "Conta cadastrada com sucesso!" );
   }
   
   /**
    * Exclui uma conta do sistema.
    */
   public void excluirConta()
   {
      final String conta = this.solicitarConta( "Insira a conta a ser excluída:" );
      
      this.fachada.excluirConta( conta );
      
      if( conta != null )
      {
         JOptionPane.showMessageDialog( null, "Conta " + conta + "excluída com sucesso!" );
      }
   }
   
   private int getCPF()
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int cpf = 0;
      
      while( !sucesso )
      {
         final String input = JOptionPane.showInputDialog( ( nÉsimaVez? "CPF inválido!\n\n" : "" )
            + "Insira um CPF:" );
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
      
      nome = JOptionPane.showInputDialog( "Insira o nome novo do acionista: \n" );
      
      return nome;
   }
   
   private double getSaldo()
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int input = 0;
      
      while( !sucesso )
      {
         final String inputString = JOptionPane.showInputDialog( ( nÉsimaVez? "Saldo inválido!\n\n"
            : "" ) + "Insira o saldo do novo acionista:" );
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
      
      nome = JOptionPane.showInputDialog( "Insira a senha do novo acionista: \n" );
      
      return nome;
   }
   
   private String solicitarConta( final String pergunta )
   {
      String nome = null;
      boolean inputError = true;
      do
      {
         nome = JOptionPane.showInputDialog( ( inputError? "" : "Usuário inválido!\n\n" )
            + this.fachada.contasTesteToString() + "\n\n" + pergunta );
         
         if( nome == null )
         {
            return null;
         }
         inputError = this.fachada.existeAConta( nome );
         
      } while( !inputError );
      
      return nome;
   }
}
