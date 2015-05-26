/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_dados;

import java.util.ArrayList;
import java.util.List;

import util.Biblioteca;

/**
 * Representa um inventário de um cliente.
 * 
 * @author Evandro  Coan
 */
public class Inventario
{
   private final static char SEPARADOR = ';';
   
   private List< Ação > listaDeAções = new ArrayList<>();
   
   /**
    * Adiciona um ação do inventario de ações do cliente.
    * 
    * @param acao a ação para se adicionar
    * @return true caso possa adicionar a ação ao inventário, false caso
    *         contrário
    */
   public boolean adicionarAoInventario( final Ação acao )
   {
      return this.listaDeAções.add( acao );
   }
   
   /**
    * @param nomeAção o nome da ação.
    * @return true se ela existe false caso contrário.
    */
   public boolean existeNoInvetário( final String nomeAção )
   {
      boolean existe = false;
      
      for( final Ação ação: this.listaDeAções )
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
   public boolean existeQuantidade( final int quantidade )
   {
      boolean existe = false;
      
      for( final Ação ação: this.listaDeAções )
      {
         if( ação.getQuantidade() >= quantidade )
         {
            existe = true;
            break;
         }
      }
      return existe;
   }
   
   /**
    * @return listaDeAções a listaDeAções em forma de um ArrayList< Ação >
    */
   public List< Ação > getListaDeAções()
   {
      return this.listaDeAções;
   }
   
   /**
    * @param nome o nome da ação a procurar o preço.
    * @return o preço da ação.
    */
   public double getPreço( final String nome )
   {
      double preço = 0;
      
      for( final Ação ação: this.listaDeAções )
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
   public int getQuantidade( final String nome )
   {
      int quantidade = 0;
      
      for( final Ação ação: this.listaDeAções )
      {
         if( ação.getNome().equals( nome ) )
         {
            quantidade = ação.getQuantidade();
         }
      }
      return quantidade;
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
      final StringBuilder enviar = new StringBuilder();
      
      for( final Ação i: this.listaDeAções )
      {
         enviar.append( i.getNome() + Inventario.SEPARADOR + " " );
         int contadorDeChar = 0;
         
         for( final char caractere: enviar.toString().toCharArray() )
         {
            if( caractere == Inventario.SEPARADOR )
            {
               contadorDeChar++;
            }
         }
         if( Biblioteca.quebrarLinha( contadorDeChar ) )
         {
            enviar.append( '\n' );
         }
      }
      return enviar.toString();
   }
   
   /**
    * Remove uma ação do inventario de ações do cliente.
    * 
    * @param acao a ação para se remover
    * @return true caso possa remover a ação do inventário, false caso contrário
    */
   public boolean removerDoInventario( final Ação acao )
   {
      return this.listaDeAções.remove( acao );
   }
   
   /**
    * Definife, e portanto sobre-escreve o
    * 
    * @param listaDeAções a listaDeAções em forma de um ArrayList< Ação >
    */
   @SuppressWarnings( "cast" )
   public void setInventario( final List< Ação > listaDeAções )
   {
      this.listaDeAções = (ArrayList< Ação >) listaDeAções;
   }
}
