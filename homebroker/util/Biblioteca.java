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
        final double random = Math.random();
        final double x = ( random * 100 ) + 1;
        final int y = (int) x;
        return y;
    }
    
    /**
     * Quera a lina a cada 10 contagens do contador.
     * 
     * @param contador número de símbolos ocorrentes.
     * @return true caso seja preciso quebrar a linha.
     */
    public static boolean quebrarLinha( final int contador )
    {
        return ( contador % 10 ) == 9;
    }
    
    /**
     * Configura as fontes de todos os componentes do componente para a fonte
     * passado como parâmetros.
     * 
     * @param componente o componente a ter a fonte trocada
     * @param novaFonte a novaFonte a ser utilizada pelos componentes
     */
    public static void trocarFontes( final Component componente,
        final Font novaFonte )
    {
        componente.setFont( novaFonte );
        if( componente instanceof Container )
        {
            for( final Component child: ( (Container) componente ).getComponents() )
            {
                Biblioteca.trocarFontes( child, novaFonte );
            }
        }
    }
}
