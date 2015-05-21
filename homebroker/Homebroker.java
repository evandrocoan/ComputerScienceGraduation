/**
 * 
 */
package homebroker;

import homebroker.interface_gráfica.JanelaDeLogin;
import homebroker.interface_gráfica.JanelaDoHomebroker;

import javax.swing.SwingUtilities;

/**
 * 
 * @author Professional
 */
public class Homebroker
{
   /**
    * Método principal que inicia a execução do programa.
    * 
    * @param args caso receba o argumento 'teste' abre o programa em uma conta
    *           teste.
    */
   public static void main( final String... args )
   {
      new Homebroker().iniciarSistema( args );
   }
   
   private Homebroker()
   {
   }
   
   /**
    * Realiza a leitura dos parâmetros passados por linha de comando. Caso não
    * haja nenhum parâmetro, inicia a interface gráfica de login no sistema.
    * 
    * @param args "Os comandos disponívels:\n" +
    *           "teste: abre o programa em mode de teste sem dica" +
    *           "das contas para se logar\n" +
    *           "dica: o mesmo que teste, mas abre o programa " +
    *           "com dicas de contas para se logar."
    */
   @SuppressWarnings( "all" )
   private void iniciarSistema( final String[] args )
   {
      if( ( args == null ) || ( args.length == 0 ) )
      {
         this.modoDeLogin();
      } else
      {
         boolean exitLoop = false;
         
         for( int i = 0; i < args.length; i++ )
         {
            switch( args[i] )
            {
            case "teste":
               this.modoDeTeste();
               break;
            
            case "dica":
               this.modoDeDica();
               break;
            
            case "ajuda":
               System.out.println( "Comandos disponívels:\n"
                        + "teste: abre o programa em mode de teste sem dica"
                        + "das contas para se logar\n"
                        + "dica: o mesmo que teste, mas abre o programa "
                        + "com dicas de contas para se logar." );
               exitLoop = true;
               break;
            
            default:
               System.out.println( "Linha de Comando inválido! " + args[i] );
               exitLoop = true;
               break;
            }
            if( exitLoop )
            {
               break;
            }
         }
      }
   }
   
   /**
    * Inicia o sistema em modo de teste exibindo dica de contas para logar.
    */
   private void modoDeDica()
   {
      /**
       * Programando um trabalho para o Event Dispatcher Thread. Porque Java
       * Swing não é thread-safe.
       */
      SwingUtilities.invokeLater( () -> {
         final JanelaDeLogin janelaDeLogin;
         final JanelaDoHomebroker janelaDoHomebroker;
         
         janelaDeLogin = JanelaDeLogin.getInstância();
         janelaDoHomebroker = JanelaDoHomebroker.getInstância();
         
         janelaDeLogin.loginNoSistema( "dica" );
         janelaDoHomebroker.setVisible( true );
      } );
   }
   
   /**
    * Inicia o sistema o login no sistema.
    */
   private void modoDeLogin()
   {
      /**
       * Programando um trabalho para o Event Dispatcher Thread. Porque Java
       * Swing não é thread-safe.
       */
      SwingUtilities.invokeLater( () -> {
         final JanelaDeLogin janelaDeLogin;
         final JanelaDoHomebroker janelaDoHomebroker;
         
         janelaDeLogin = JanelaDeLogin.getInstância();
         janelaDoHomebroker = JanelaDoHomebroker.getInstância();
         
         janelaDeLogin.loginNoSistema( "login" );
         janelaDoHomebroker.setVisible( true );
      } );
   }
   
   /**
    * Inicia o sistema em modo de teste.
    */
   private void modoDeTeste()
   {
      /**
       * Programando um trabalho para o Event Dispatcher Thread. Porque Java
       * Swing não é thread-safe.
       */
      SwingUtilities.invokeLater( () -> {
         final JanelaDeLogin janelaDeLogin;
         final JanelaDoHomebroker janelaDoHomebroker; 
         
         janelaDeLogin = JanelaDeLogin.getInstância();
         janelaDoHomebroker = JanelaDoHomebroker.getInstância();
         
         janelaDeLogin.loginNoSistema( "teste" );
         janelaDoHomebroker.setVisible( true );
      } );
   }
}
