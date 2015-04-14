/**
 * Pacote que contém a classe principal de testes.
 */
package testes;

import homebroker.interface_gráfica.Homebroker;

/**
 * Esta classe inicia o programa principal em mode de teste.
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
    public static void main( String... args )
    {
        Homebroker.main( "teste" );
    }
}
