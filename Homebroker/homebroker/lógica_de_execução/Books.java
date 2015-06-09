/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * Inicia a interface gráfica que exibe o book de ofertas com as ordens de compra e venda sendo
 * feitas em tempo real.
 *
 * @author Professional
 */
public final class Books
{
   private static Books INSTÂNCIA;
   
   private static final Logger LOG;
   
   static
   {
      LOG = Logger.getLogger( Books.class.getName() );
   }
   
   /**
    * As ofertas do mercado realizadas.
    */
   private final List< Oferta > ofertas;
   
   /**
    * As vendas do mercado realizados.
    */
   private final List< Venda > vendas;
   
   @SuppressWarnings( "unused" )
   private final Oferta oferta = null;
   @SuppressWarnings( "unused" )
   private final Venda venda = null;
   
   /**
    * Construtor do objeto para implementação do padrão de projeto Singleton.
    */
   private Books()
   {
      Books.LOG.setLevel( Level.OFF );
      
      this.ofertas = new ArrayList<>();
      this.vendas = new ArrayList<>();
   }
   
   /**
    * Serve para implementação do padrão de projeto singleton.
    *
    * @return INSTÂNCIA a única instância do BookDeOfertas.
    */
   public static Books getInstância()
   {
      synchronized( Books.class )
      {
         if( Books.INSTÂNCIA == null )
         {
            Books.INSTÂNCIA = new Books();
         }
      }
      return Books.INSTÂNCIA;
   }
   
   /**
    * @param preço o preço da oferta
    * @param quantidade a quantidade
    * @param nome o nome da ação
    * @param conta a conta qual faz oferta
    * @return true se a oferta foi adicionada com sucesso.
    * */
   public boolean adicionarOfertaDeCompra( final double preço, final int quantidade,
      final String nome, final Conta conta )
   {
      final Oferta novaOferta = new Oferta( preço, quantidade, nome, "Compra", conta );
      final boolean resultado = this.ofertas.add( novaOferta );
      this.realizarVenda( novaOferta );
      return resultado;
   }
   
   /**
    * Cria uma ordem de venda de uma ação no book de ofertas.
    *
    * @param preço o preço da oferta
    * @param quantidade a quantidade
    * @param nome o nome da ação
    * @param conta a conta qual faz oferta
    * @return true se a oferta foi adicionada com sucesso.
    */
   public boolean adicionarOfertaDeVenda( final double preço, final int quantidade,
      final String nome, final Conta conta )
   {
      final Oferta novaOferta = new Oferta( preço, quantidade, nome, "Venda", conta );
      final boolean resultado = this.ofertas.add( novaOferta );
      this.realizarVenda( novaOferta );
      return resultado;
   }
   
   /**
    * @param conta a conta no qual terá suas ordens canceladas.
    */
   public void cancelarOfertas( final Conta conta )
   {
      for( int i = 0; i < this.ofertas.size(); i++ )
      {
         final Oferta oferta = this.ofertas.get( i );
         
         if( ( conta ).equals( oferta.getConta() ) )
         {
            oferta.setUtilidade();
         }
      }
   }
   
   /**
    * Dado o código de uma oferta, informa se existem novas ofertas lançadas no mercado a partir da
    * oferta informada.
    *
    * @param ultimaOferta a última oferta visualizada
    * @return true se existem novas ofertas, false caso contrário.
    */
   public boolean existemNovasOfertas( final int ultimaOferta )
   {
      final int númeroDeOfertas = this.ofertas.size() - 1;
      
      if( Books.LOG.isLoggable( Level.SEVERE ) )
      {
         Books.LOG
            .severe( "1 - númeroDeOfertas < ultimaOferta = " + ( númeroDeOfertas < ultimaOferta )
               + "(" + númeroDeOfertas + "<" + ultimaOferta + ")" );
      }
      if( númeroDeOfertas < ultimaOferta )
      {
         return false;
      }
      if( Books.LOG.isLoggable( Level.SEVERE ) )
      {
         Books.LOG.severe( "2 - númeroDeOfertas > ultimaOferta = "
            + ( númeroDeOfertas > ultimaOferta ) );
      }
      return númeroDeOfertas > ultimaOferta;
   }
   
   /**
    * @param indice qual oferta buscar
    * @return açãoEmOferta uma String representando uma ação em oferta.
    */
   public String ofertaToString( final int indice )
   {
      return this.ofertas.get( indice ).ofertaToString();
   }
   
   synchronized private void realizarVenda( final Oferta oferta )
   {
      if( oferta.isUtilidade() )
      {
         if( oferta.getTipoDeOferta().equals( "Venda" ) )
         {
            this.realizarVendaProcura1( oferta );
            
         } else
         {
            this.realizarVendaProcura2( oferta );
         }
      }
   }
   
   synchronized private void realizarVendaAjuste( final Oferta venda, final Oferta compra )
   {
      /* Caso a quantidade de venda seja maior que a quantidade de compra, devemos criar uma nova
       * ordem de venda, contendo o preço da venda, e a diferença entre a venda e a compra. Isso em
       * nome da conta de venda. */
      if( venda.getQuantidade() > compra.getQuantidade() )
      {
         venda.ajustarSaldo( venda.getPreço() * compra.getQuantidade() );
         compra.ajustarSaldo( -venda.getPreço() * compra.getQuantidade() );
         
         this.adicionarOfertaDeVenda( venda.getPreço(),
            venda.getQuantidade() - compra.getQuantidade(), venda.getAção(), venda.getConta() );
         
         this.vendas.add( new Venda( venda, compra, venda.getPreço(), compra.getQuantidade() ) );
      }
      /* Caso a quantidade de venda seja menor que a quantidade de compra, devemos criar uma nova
       * ordem de compra, contendo o preço da compra, e a diferença entre a venda e a compra. Isso
       * em nome da conta de compra. */
      if( venda.getQuantidade() < compra.getQuantidade() )
      {
         venda.ajustarSaldo( venda.getPreço() * venda.getQuantidade() );
         compra.ajustarSaldo( -venda.getPreço() * venda.getQuantidade() );
         
         this.adicionarOfertaDeCompra( compra.getPreço(),
            compra.getQuantidade() - venda.getQuantidade(), compra.getAção(), compra.getConta() );
         
         this.vendas.add( new Venda( venda, compra, venda.getPreço(), venda.getQuantidade() ) );
      }
      if( venda.getQuantidade() == compra.getQuantidade() )
      {
         venda.ajustarSaldo( venda.getPreço() * venda.getQuantidade() );
         compra.ajustarSaldo( -compra.getPreço() * compra.getQuantidade() );
         
         this.vendas.add( new Venda( venda, compra, venda.getPreço(), venda.getQuantidade() ) );
      }
   }
   
   synchronized private void realizarVendaProcura1( final Oferta venda )
   {
      for( int index = 0; index < this.ofertas.size(); index++ )
      {
         final Oferta compra = this.ofertas.get( index );
         
         if( !compra.getAção().equals( venda.getAção() ) || !compra.isUtilidade() )
         {
            continue;
         }
         if( compra.getTipoDeOferta().equals( "Compra" ) )
         {
            if( venda.getPreço() <= compra.getPreço() )
            {
               venda.setUtilidade();
               compra.setUtilidade();
               this.realizarVendaAjuste( venda, compra );
               break;
            }
         }
      }
   }
   
   synchronized private void realizarVendaProcura2( final Oferta compra )
   {
      for( int index = 0; index < this.ofertas.size(); index++ )
      {
         final Oferta venda = this.ofertas.get( index );
         
         if( !venda.getAção().equals( compra.getAção() ) || !venda.isUtilidade() )
         {
            continue;
         }
         if( venda.getTipoDeOferta().equals( "Venda" ) )
         {
            if( compra.getPreço() >= venda.getPreço() )
            {
               venda.setUtilidade();
               compra.setUtilidade();
               this.realizarVendaAjuste( venda, compra );
               break;
            }
         }
      }
   }
   
   public String vendaToString( final int indice )
   {
      return this.vendas.get( indice ).vendaToString();
   }
}
