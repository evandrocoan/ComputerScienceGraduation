/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 *
 * @author Professional
 */
public final class JanelaDeLogin extends JFrame
{
   /**
    * Contém a única instância desta classe.
    */
   private static JanelaDeLogin INSTÂNCIA;
   
   private final Fachada fachada;
   
   private JanelaDoHomebroker janela = null;
   
   /**
    * Construtor que cria a janela principal do programa.
    */
   private JanelaDeLogin()
   {
      super( "Simulador de HomeBroker" );
      this.fachada = Fachada.getInstância();
      
      // Define que a janela deve fechar ao sair.
      this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
      
      // Abre a janela maximizado
      this.setLocation( 110, 10 );
      // this.setLocation( 250, 150 );
      // this.setExtendedState( Frame.MAXIMIZED_BOTH );
      
      // Ajusta a janela ao tamanho dos elementos.
      this.pack();
   }
   
   /**
    * @return instância a instância da janela.
    */
   public static JanelaDeLogin getInstância()
   {
      synchronized( JanelaDeLogin.class )
      {
         if( JanelaDeLogin.INSTÂNCIA == null )
         {
            JanelaDeLogin.INSTÂNCIA = new JanelaDeLogin();
         }
      }
      return JanelaDeLogin.INSTÂNCIA;
   }
   
   /**
    * Método de realiza o login no sistema.
    *
    * @param modo uma dica que será aprensetada no menu do login. Inicialmente ela serve para exibir
    *           quais contas estão disponíveis para login.
    */
   @SuppressWarnings( "all" )
   public void loginNoSistema( final String modo )
   {
      if( this.janela != null )
      {
         this.janela.finalize();
      }
      switch( modo )
      {
      case "login":
         this.loginNoSistemaInterno( "" );
         break;
      
      case "teste":
         this.fachada.loginNoSistemaChecagem( "admin", "admin" );
         break;
      
      case "dica":
         final String dica = '\n' + this.fachada.contasToString();
         this.loginNoSistemaInterno( dica.toString() );
         break;
      
      default:
         System.out.println( "Comando de Login inválido! " + modo );
         System.exit( 0 );
         break;
      }
      this.janela = JanelaDoHomebroker.getInstância( true );
   }
   
   /**
    * @param dica
    * @param fachada
    */
   private void loginNoSistemaInterno( final String dica )
   {
      String usuário = "";
      String senha = "";
      boolean inputError = true;
      do
      {
         usuário = JOptionPane.showInputDialog( ( inputError? ""
            : "Usuário ou senha inválidos ou bloqueados!\n\n" )
            + "Insira qual conta será feito login: " + dica );
         
         if( ( usuário == null ) )
         {
            break;
         }
         senha = JOptionPane.showInputDialog( "Insira qual senha para a " + "conta: " + usuário );
         
         if( ( senha == null ) )
         {
            break;
         }
         inputError = this.fachada.loginNoSistemaChecagem( usuário, senha );
         
      } while( !inputError );
      
      if( ( usuário == null ) | ( senha == null ) )
      {
         System.exit( 0 );
      }
   }
}
