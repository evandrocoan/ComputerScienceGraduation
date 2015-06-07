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
   
   public void alterarSenhaMinhaSenha()
   { // TODO
      String novaSenha = "1";
      String novaSenha2 = "2";
      String senha = null;
      boolean error = false;
      do
      {
         final String mensagem = ( error? "Senha inválida!\n" : "" )
            + "Bem vindo ao subsistema de troca de senha!\n"
            + "Para continuar insira sua senha atual: \n";
         senha = JOptionPane.showInputDialog( mensagem );
         
         if( this.fachada.checarSenha( senha ) )
         {
            error = false;
            
            while( !novaSenha2.equals( novaSenha ) )
            {
               final String menssagem = ( error? "As senhas não conferem!\n" : "" )
                  + "Insira sua nova senha: \n";
               novaSenha = JOptionPane.showInputDialog( menssagem );
               
               if( novaSenha == null )
               {
                  return;
               }
               novaSenha2 = JOptionPane.showInputDialog( "Insira sua nova senha novamente: \n" );
               
               if( novaSenha2 == null )
               {
                  return;
               }
               error = true;
            }
         }
         error = true;
         
         if( senha == null )
         {
            return;
         }
      } while( !this.fachada.checarSenha( senha ) );
      
      this.fachada.setSenha( novaSenha );
      JOptionPane.showMessageDialog( null, "Senha alterada com sucesso!" );
   }
   
   /**
    * Efetua o bloqueio de uma conta.
    */
   public void efetuarBloqueio()
   {
      if( this.fachada.estáAutorizado() )
      {
         final String conta = this.solicitarConta( "\n\nInsira qual conta será bloqueada: ", true );
         
         if( conta != null )
         {
            this.fachada.bloquearConta( conta );
            JOptionPane.showMessageDialog( null, "Bloqueio realizado com sucesso!\n\n"
               + this.fachada.contasToString() );
         }
      }
   }
   
   /**
    * Efetua o cadastro de uma nova conta no Sistema.
    */
   public void efetuarCadastro()
   {
      if( this.fachada.estáAutorizado() )
      {
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
         JOptionPane.showMessageDialog( null,
            "Conta cadastrada com sucesso!\n\n" + this.fachada.contasToString() );
      }
   }
   
   /**
    * Exclui uma conta do sistema.
    */
   public void excluirConta()
   {
      if( this.fachada.estáAutorizado() )
      {
         final String conta = this.solicitarConta( "Insira a conta a ser excluída:", true );
         
         this.fachada.excluirConta( conta );
         
         if( conta != null )
         {
            JOptionPane.showMessageDialog( null, "Conta " + conta + "excluída com sucesso!\n\n"
               + this.fachada.contasToString() );
         }
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
      return JOptionPane.showInputDialog( "Insira a senha do novo acionista: \n" );
   }
   
   /**
    * Efetua a remoção dos privilégios de administrador de uma conta.
    */
   public void removerPrivilégios()
   {
      if( this.fachada.estáAutorizado() )
      {
         final String conta = this.solicitarConta( "\n\nInsira qual conta perderá "
            + "privilégios: ", true );
         
         if( conta != null )
         {
            this.fachada.ajustarPrivilégios( conta, false );
            JOptionPane.showMessageDialog( null, "Remoção realizado com sucesso!\n\n"
               + this.fachada.contasToString() );
         }
      }
   }
   
   private String solicitarConta( final String pergunta, final boolean imunidade )
   {
      String nome = null;
      boolean inputError = true;
      do
      {
         nome = JOptionPane.showInputDialog( ( inputError? "" : "Usuário inválido!\n\n" )
            + this.fachada.contasToString() + pergunta );
         
         if( nome == null )
         {
            return null;
         }
         inputError = this.fachada.existeAConta( nome );
         if( imunidade )
         {
            inputError = !this.fachada.estáLogadoAgora( nome );
         }
         
      } while( !inputError );
      
      return nome;
   }
}
