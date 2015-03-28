/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

/**
 * Biblioteca de funções para o funcionamento do Homebroker.
 * 
 * @author Professional
 */
public class Biblioteca
{
    /**
     * Gera número aleátorios entre 1 e 100
     * 
     * @return numero um número aleátorio entre 1 e 100
     */
    public static int gerarNumeroAleatorio()
    {
        double random = Math.random();
        double x = random * 100 + 1;
        int y = (int) x;
        return y;
    }
}
