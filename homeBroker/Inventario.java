package homeBroker;

import java.util.ArrayList;

import javax.swing.JOptionPane;

/**
 * Representa um inventário de um cliente.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class Inventario
{
    /**
     * Exibe na tela o inventário do usuário
     * 
     * @param conta a conta para qual será exibido o inventário
     */
    public static void exibirInventario( Conta conta )
    {
        String teste = conta.getInventario().inventarioToString();
        JOptionPane.showMessageDialog( null, teste );
    }
    
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
        String enviar = new String();
        for( Ação i: this.listaDeAções )
        {
            enviar = enviar + i.getNome() + ", ";
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