/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_dados;

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
     * Serve para corrigir um bug no gerador automático de diagramas que não
     * reconhece a composição feita acima do ArrayList de Ações.
     */
    public Ação inútil = new Ação( 0, 0, null );
    
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
    
    /**
     * @param nome o nome da ação a procurar o preço.
     * @return o preço da ação.
     */
    public double getAçãoPreço( String nome )
    {
        double preço = 0;
        
        for( Ação ação: this.listaDeAções )
        {
            if( ( ação.getNome() ).equals( nome ) )
            {
                preço = ação.getPreço();
            }
        }
        return preço;
    }
    
    /**
     * @param nome o nome da ação.
     * @return quantidade a quantidade de ações existentes no invetário.
     */
    public int getAçãoQuantidade( String nome )
    {
        int quantidade = 0;
        
        for( Ação ação: this.listaDeAções )
        {
            if( ação.getNome().equals( nome ) )
            {
                quantidade = ação.getQuantidade();
            }
        }
        return quantidade;
    }
    
    /**
     * @param nomeAção o nome da ação.
     * @return true se ela existe false caso contrário.
     */
    public boolean existeAçãoNoInvetário( String nomeAção )
    {
        boolean existe = false;
        
        for( Ação ação: this.listaDeAções )
        {
            if( ação.getNome().equals( nomeAção ) )
            {
                existe = true;
                break;
            }
        }
        return existe;
    }
    
    /**
     * @param quantidade a quantidade de ações.
     * @return true se existe a quantidade de ações especificada.
     */
    public boolean existeQuantidadeNoInvetário( int quantidade )
    {
        boolean existe = false;
        
        for( Ação ação: this.listaDeAções )
        {
            if( ação.getQuantidade() >= quantidade )
            {
                existe = true;
                break;
            }
        }
        return existe;
    }
}