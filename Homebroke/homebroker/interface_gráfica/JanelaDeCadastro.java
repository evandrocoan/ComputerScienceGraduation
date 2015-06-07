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
    * Efetua a remoção dos privilégios de administrador de uma conta.
    */
   public void adicionarPrivilégios()
   {
      if( this.isAdministradora() )
      {
         final String conta = this.solicitarConta( "Insira qual conta ganhará " + "privilégios: ",
            true );
         
         if( conta != null )
         {
            this.fachada.ajustarPrivilégios( conta, true );
            JOptionPane.showMessageDialog( null,
               "Adesão realizado com sucesso!\n\n" + this.fachada.contasToString() );
         }
      }
   }
   
   public void alterarSenha( final String conta )
   {
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
         
         if( this.fachada.checarSenha( senha, null ) )
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
      } while( !this.fachada.checarSenha( senha, null ) );
      
      this.fachada.setSenha( novaSenha, conta );
      JOptionPane.showMessageDialog( null, "Senha alterada com sucesso!" );
   }
   
   /**
    * Permite administrador alterarem a senha de qualquer conta.
    */
   public void alterarSenhas()
   {
      if( this.isAdministradora() )
      {
         final String conta = this.solicitarConta( "Insira qual conta terá a senha modificada: ",
            false );
         if( conta == null )
         {
            return;
         }
         this.alterarSenha( conta );
      }
   }
   
   /**
    * Efetua o bloqueio de uma conta.
    */
   public void efetuarBloqueio()
   {
      if( this.isAdministradora() )
      {
         final String conta = this.solicitarConta( "Insira qual conta será bloqueada: ", true );
         
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
      if( this.isAdministradora() )
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
      if( this.isAdministradora() )
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
    * @return
    */
   private boolean isAdministradora()
   {
      if( !this.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há nenhuma conta carregada no sistema!" );
         return false;
      }
      if( !this.fachada.isAdministradora() )
      {
         JOptionPane.showMessageDialog( null, "Acesso negado! "
            + "Você precisa ter privilégio de administrador." );
         return false;
      }
      return true;
   }
   
   /**
    * Efetua a remoção dos privilégios de administrador de uma conta.
    */
   public void removerPrivilégios()
   {
      if( this.isAdministradora() )
      {
         final String conta = this.solicitarConta( "Insira qual conta perderá " + "privilégios: ",
            true );
         
         if( conta != null )
         {
            this.fachada.ajustarPrivilégios( conta, false );
            JOptionPane.showMessageDialog( null, "Remoção realizado com sucesso!\n\n"
               + this.fachada.contasToString() );
         }
      }
   }
   
   /**
    * @param pergunta a pergunta feita ao usuário.
    * @param imunidade true caso não se possa alterar a própria conta.
    * @return o nome de uma conta válida do sistema.
    */
   private String solicitarConta( final String pergunta, final boolean imunidade )
   {
      String nome = null;
      boolean inputError = true;
      boolean error = false;
      boolean mensagem = false;
      do
      {
         nome = JOptionPane.showInputDialog( ( mensagem? "Usuário inválido!\n\n" : "" ) + pergunta
            + "\n\n" + this.fachada.contasToString() );
         
         if( nome == null )
         {
            return null;
         }
         inputError = this.fachada.existeAConta( nome );
         if( imunidade )
         {
            error = this.fachada.estáLogadoAgora( nome );
         }
         mensagem = true;
         
      } while( !inputError || error );
      
      return nome;
   }
}
