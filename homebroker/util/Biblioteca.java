/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.util;

import java.awt.Component;
import java.awt.Container;
import java.awt.Font;

/**
 * Biblioteca de funções para o funcionamento do Homebroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
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
    
    /**
     * Configura as fontes de todos os componentes do componente para a fonte
     * passado como parâmetros.
     * 
     * @param componente o componente a ter a fonte trocada
     * @param novaFonte a novaFonte a ser utilizada pelos componentes
     */
    public static void trocarFontes( Component componente, Font novaFonte )
    {
        componente.setFont( novaFonte );
        if( componente instanceof Container )
        {
            for( Component child: ( (Container) componente ).getComponents() )
            {
                trocarFontes( child, novaFonte );
            }
        }
    }
}
