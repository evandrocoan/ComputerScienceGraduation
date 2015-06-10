/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker;

import homebroker.interface_gráfica.JanelaDeLogin;
import homebroker.lógica_de_execução.Books;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Professional
 */
public final class Homebroker
{
   private static final Logger LOG;
   
   static
   {
      LOG = Logger.getLogger( Books.class.getName() );
      Homebroker.LOG.setLevel( Level.OFF );
   }
   
   private final JanelaDeLogin janela;
   
   private Homebroker()
   {
      this.janela = JanelaDeLogin.getInstância();
   }
   
   public static Logger getLOG()
   {
      return Homebroker.LOG;
   }
   
   /**
    * Método principal que inicia a execução do programa.
    *
    * @param args caso receba o argumento 'teste' abre o programa em uma conta teste.
    */
   public static void main( final String... args )
   {
      new Homebroker().iniciarSistema( args );
   }
   
   /**
    * Realiza a leitura dos parâmetros passados por linha de comando. Caso não haja nenhum
    * parâmetro, inicia a interface gráfica de login no sistema.
    *
    * @param args "Os comandos disponíveis:\n" + "teste: abre o programa em mode de teste sem dica"
    *           + " das contas para se logar\n" + "dica: o mesmo que teste, mas abre o programa " +
    *           "com dicas de contas para se logar."
    */
   @SuppressWarnings( "all" )
   private void iniciarSistema( final String[] args )
   {
      if( ( args == null ) || ( args.length == 0 ) )
      {
         this.janela.loginNoSistema( "dica" );
      } else
      {
         boolean exitLoop = false;
         
         for( int i = 0; i < args.length; i++ )
         {
            switch( args[i] )
            {
            case "teste":
               this.janela.loginNoSistema( "teste" );
               break;
            
            case "dica":
               this.janela.loginNoSistema( "dica" );
               break;
            
            case "ajuda":
               System.out.println( "Comandos disponíveis:\n"
                  + "teste: abre o programa em mode de teste sem dica"
                  + " das contas para se logar\n" + "dica: o mesmo que teste, mas abre o programa "
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
}
