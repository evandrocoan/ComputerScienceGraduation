/**
 * Esta classe serve como utiliário que realiza uma serie de test para com a
 * primeira interação do HomeBroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class DriverClass
{
    
    /**
     * Método principal que inicia a execução dos testes
     * 
     * @param args os argumentos passados por linha de comando
     */
    public static void main( String[] args )
    {
        Conta contaTeste =
                new Conta( "Evandro", "123", 2000.5, true, new Inventario() );
        
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( 20.2, 1000, "Tabajara SA" ) );
        String teste = contaTeste.getInventario().inventarioToString();
        
        System.out.println( teste );
    }
}
