import java.util.ArrayList;

/**
 * Representa um inventário de um cliente.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class Inventario
{
    private ArrayList< Acao > inventario = new ArrayList< Acao >();
    
    /**
     * Retorna o inventário representado como uma String. Essa String é composta
     * pelos nomes das ações no inventário.
     * 
     * @return inventario o inventário reprensentado como um String
     */
    public String inventarioToString()
    {
        String enviar = new String();
        for( Acao i: inventario )
        {
            enviar = enviar + i.getAcaoNome() + ", ";
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
    public boolean adicionarAoInventario( Acao acao )
    {
        return this.inventario.add( acao );
    }
    
    /**
     * Remove uma ação do inventario de ações do cliente.
     * 
     * @param acao a ação para se remover
     * @return true caso possa remover a ação do inventário, false caso
     *         contrário
     */
    public boolean removerDoInventario( Acao acao )
    {
        return this.inventario.remove( acao );
    }
}