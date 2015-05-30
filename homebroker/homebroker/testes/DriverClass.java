/**
 * Pacote que contém a classe principal de testes.
 */
package homebroker.testes;

import homebroker.Homebroker;

/**
 * Esta classe inicia o programa principal em mode de teste.
 *
 * @author Professional
 */
public final class DriverClass
{
   /**
    * Torna esse uma classe de utilidades, impedindo sua instanciação.
    */
   private DriverClass()
   {
   }
   
   /**
    * Método principal que inicia a execução dos testes
    *
    * @param args os argumentos passados por linha de comando
    */
   public static void main( final String... args )
   {
      Homebroker.main( "teste" );
   }
}
