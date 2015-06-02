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
   private static final Logger LOG = Logger.getLogger( BookDeOfertas.class
            .getName() );
   
   /**
    * Por padrão, este tipo de instanciação é thread safe.
    */
   private static final BookDeOfertas INSTÂNCIA = new BookDeOfertas();
   
   /**
    * As ofertas do mercado realizadas.
    */
   private final List< OfertaDoMercado > ofertasDoMercado;
   
   /**
    * Construtor do objeto para implementação do padrão de projeto Singleton.
    */
   private BookDeOfertas()
   {
      BookDeOfertas.LOG.setLevel( Level.OFF );
      this.ofertasDoMercado = new ArrayList<>();
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
   public boolean adicionarOfertaDeCompra( final double preço,
            final int quantidade, final String nome, final Conta conta )
   {
      return this.ofertasDoMercado.add( new OfertaDoMercado( preço, quantidade,
               nome, "Compra", conta ) );
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
   public boolean adicionarOfertaDeVenda( final double preço,
            final int quantidade, final String nome, final Conta conta )
   {
      if( BookDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         BookDeOfertas.LOG.fine( "Estou em: " + this.getClass()
                  + "adicionarOfertaDeVenda" );
      }
      return this.ofertasDoMercado.add( new OfertaDoMercado( preço, quantidade,
               nome, "Venda", conta ) );
   }
   
   /**
    * @param conta a conta no qual terá suas ordens canceladas.
    */
   public void cancelarOfertas( final Conta conta )
   {
      for( int i = 0; i < this.ofertasDoMercado.size(); i++ )
      {
         final OfertaDoMercado oferta = this.ofertasDoMercado.get( i );
         
         if( ( conta ).equals( oferta.getConta() ) )
         {
            this.ofertasDoMercado.remove( oferta );
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
      final int númeroDeOfertas = this.ofertasDoMercado.size() - 1;
      
      if( BookDeOfertas.LOG.isLoggable( Level.SEVERE ) )
      {
         BookDeOfertas.LOG.severe( "1 - númeroDeOfertas < ultimaOferta = "
                  + ( númeroDeOfertas < ultimaOferta ) + "(" + númeroDeOfertas
                  + "<" + ultimaOferta + ")" );
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
      return this.ofertasDoMercado.get( indice ).ofertaToString();
   }
}
