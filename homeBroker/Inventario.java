/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

import java.util.ArrayList;

/**
 * Representa um inventário de um cliente.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class Inventario
{
    private ArrayList< Ação > listaDeAções = new ArrayList<>();
    
    /**
     * @return listaDeAções a listaDeAções em forma de um ArrayList< Ação >
     */
    public ArrayList< Ação > getListaDeAções()
    {
        return this.listaDeAções;
    }
    
    /**
     * Definife, e portanto sobre-escreve o
     * 
     * @param listaDeAções a listaDeAções em forma de um ArrayList< Ação >
     */
    public void setInventario( ArrayList< Ação > listaDeAções )
    {
        this.listaDeAções = listaDeAções;
    }
    
    /**
     * Retorna o inventário representado como uma String. Essa String é composta
     * pelos nomes das ações no inventário.
     * 
     * @return inventario o inventário reprensentado como um String
     */
    public String inventarioToString()
    {
        if( this.listaDeAções.size() == 0 )
        {
            return "Não há ações nesta conta";
        }
        
        String enviar = new String();
        for( Ação i: this.listaDeAções )
        {
            enviar = enviar + i.getNome() + "; ";
            
            int contadorDeChar = 0;
            for( char caractere: enviar.toCharArray() )
            {
                if( caractere == ';' )
                {
                    contadorDeChar++;
                }
            }
            
            if( contadorDeChar % 10 == 9 )
            {
                enviar = enviar + "\n";
            }
        }
        return enviar;
    }
    
    /**
     * Adiciona um ação do inventario de ações do cliente.
     * 
     * @param acao a ação para se adicionar
     * @return true caso possa adicionar a ação ao inventário, false caso
     *         contrário
     */
    public boolean adicionarAoInventario( Ação acao )
    {
        return this.listaDeAções.add( acao );
    }
    
    /**
     * Remove uma ação do inventario de ações do cliente.
     * 
     * @param acao a ação para se remover
     * @return true caso possa remover a ação do inventário, false caso
     *         contrário
     */
    public boolean removerDoInventario( Ação acao )
    {
        return this.listaDeAções.remove( acao );
    }
}