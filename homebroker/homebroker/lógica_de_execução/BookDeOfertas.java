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
 * Inicia a interface gráfica que exibe o book de ofertas com as ordens de
 * compra e venda sendo feitas em tempo real.
 *
 * @author Professional
 */
public final class BookDeOfertas
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser
    * instanciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instanciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( BookDeOfertas.class.getName() );
   
   /**
    * Por padrão, este tipo de instanciação é thread safe.
    */
   private static final BookDeOfertas INSTÂNCIA = new BookDeOfertas();
   
   /**
    * As ofertas do mercado realizadas.
    */
   private final List< Oferta > ofertas;
   
   /**
    * As vendas do mercado realizados.
    */
   private final List< Venda > vendas;
   
   /**
    * Construtor do objeto para implementação do padrão de projeto Singleton.
    */
   private BookDeOfertas()
   {
      BookDeOfertas.LOG.setLevel( Level.OFF );
      this.ofertas = new ArrayList<>();
      this.vendas = new ArrayList<>();
   }
   
   /**
    * Serve para implementação do padrão de projeto singleton.
    *
    * @return INSTANCE a única instância existe do BookDeOfertas.
    */
   public static BookDeOfertas getInstância()
   {
      return BookDeOfertas.INSTÂNCIA;
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
      if( BookDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         BookDeOfertas.LOG.fine( "Estou em: " + this.getClass() + "adicionarOfertaDeVenda" );
      }
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
            this.ofertas.remove( oferta );
         }
      }
   }
   
   /**
    * Dado o código de uma oferta, informa se existem novas ofertas lançadas no
    * mercado a partir da oferta informada.
    *
    * @param ultimaOferta a última oferta visualizada
    * @return true se existem novas ofertas, false caso contrário.
    */
   public boolean existemNovasOfertas( final int ultimaOferta )
   {
      final int númeroDeOfertas = this.ofertas.size() - 1;
      
      if( BookDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         BookDeOfertas.LOG
            .severe( "1 - númeroDeOfertas < ultimaOferta = " + ( númeroDeOfertas < ultimaOferta )
               + "(" + númeroDeOfertas + "<" + ultimaOferta + ")" );
      }
      if( númeroDeOfertas < ultimaOferta )
      {
         return false;
      }
      if( BookDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         BookDeOfertas.LOG.severe( "2 - númeroDeOfertas > ultimaOferta = "
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
   
   private void realizarVenda( final Oferta oferta )
   {
      if( oferta.getTipoDeOferta().equals( "Venda" ) )
      {
         this.realizarVendaProcura1( oferta );
         
      } else
      {
         this.realizarVendaProcura2( oferta );
      }
   }
   
   private void realizarVendaAjuste( final Oferta venda, final Oferta compra )
   {
      final Conta conta1 = venda.getConta();
      final Conta conta2 = compra.getConta();
      final Ação ação1 = venda.getAção();
      final Ação ação2 = compra.getAção();
      
      if( ação1.getQuantidade() > ação2.getQuantidade() )
      {
         conta1.setSaldo( conta1.getSaldo() + ( ação1.getPreço() * ação1.getQuantidade() ) );
         conta2.setSaldo( conta2.getSaldo() - ( ação1.getPreço() * ação1.getQuantidade() ) );
         
         this.adicionarOfertaDeVenda( ação1.getPreço(),
            ação1.getQuantidade() - ação2.getQuantidade(), ação1.getNome(), conta1 );
      }
      if( ação1.getQuantidade() < ação2.getQuantidade() )
      {
         conta1.setSaldo( conta1.getSaldo() + ( ação1.getPreço() * ação1.getQuantidade() ) );
         conta2.setSaldo( conta2.getSaldo() - ( ação1.getPreço() * ação1.getQuantidade() ) );
         
         this.adicionarOfertaDeCompra( ação2.getPreço(),
            ação2.getQuantidade() - ação1.getQuantidade(), ação2.getNome(), conta2 );
      }
      if( ação1.getQuantidade() == ação2.getQuantidade() )
      {
         conta1.setSaldo( conta1.getSaldo() + ( ação1.getPreço() * ação1.getQuantidade() ) );
         conta2.setSaldo( conta2.getSaldo() - ( ação1.getPreço() * ação1.getQuantidade() ) );
      }
   }
   
   private void realizarVendaProcura1( final Oferta venda )
   {
      for( int index = 0; index < this.ofertas.size(); index++ )
      {
         final Oferta compra = this.ofertas.get( index );
         
         if( venda.getAção().getPreço() <= compra.getAção().getPreço() )
         {
            this.vendas.add( new Venda( venda, compra ) );
            this.realizarVendaAjuste( venda, compra );
         }
      }
   }
   
   private void realizarVendaProcura2( final Oferta compra )
   {
      for( int index = 0; index < this.ofertas.size(); index++ )
      {
         final Oferta venda = this.ofertas.get( index );
         
         if( compra.getAção().getPreço() >= venda.getAção().getPreço() )
         {
            this.vendas.add( new Venda( compra, venda ) );
            this.realizarVendaAjuste( venda, compra );
         }
      }
   }
}
