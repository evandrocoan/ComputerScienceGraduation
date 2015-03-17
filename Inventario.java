import java.util.ArrayList;

/**
 * Representa um inventário de um cliente.
 */
public class Inventario
{
    private ArrayList< Acao > acoes = new ArrayList< Acao >();
    
    /**
     * Retorna o inventário representado como uma String. Essa String é composta
     * pelos nomes das ações no inventário.
     * 
     * @return inventario o inventário reprensentado como um String
     */
    public String inventarioToString()
    {
        String enviar = "\n";
        for ( Acao  i : acoes )
        {
            enviar = enviar + "\n" + i.acaoNames() + " ";
        }
        return enviar;
    }
}