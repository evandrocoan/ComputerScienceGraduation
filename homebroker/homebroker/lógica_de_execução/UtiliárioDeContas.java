/**
 * 
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.Ação;
import homebroker.lógica_de_dados.Conta;
import homebroker.lógica_de_dados.Inventario;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import util.Biblioteca;

/**
 * 
 * @author Evandro  Coan
 */
public final class UtiliárioDeContas
{
   /**
    * Resposável por realizar o debug do programa, quando ativado. Deve ser
    * instânciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instânciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( "MotorDoHomebroker" );
   
   /**
    * Serve para tornar a classe uma classe utilitária.
    */
   private UtiliárioDeContas()
   {
      UtiliárioDeContas.LOG.setLevel( Level.OFF );
   }
   
   public static boolean bloquearConta( final String nome,
            final List< Conta > contas )
   {
      for( final Conta conta: contas )
      {
         if( conta.getNome().equals( nome ) )
         {
            return conta.definirBloqueada();
         }
      }
      return false;
   }
   
   /**
    * Transforma um ArrayList de contas e uma String
    * 
    * @param contas um ArrayList contendo as contas
    * @return texto um texto contendo os nomes das contas de teste criadas
    */
   public static String contasTesteToString( final List< Conta > contas )
   {
      final StringBuffer texto = new StringBuffer();
      
      int contador = 0;
      for( final Conta conta: contas )
      {
         texto.append( conta.getNome() ).append(
                  ( conta.isBloqueada()? "(1)" : "(0)" ) );
         texto.append( ", " );
         if( Biblioteca.quebrarLinha( contador ) )
         {
            texto.append( '\n' );
         }
         contador = contador + 1;
      }
      return texto.append( "\n(1) = Bloqueada, (0) = Desbloqueada." )
               .toString();
   }
   
   /**
    * Cria contas teste para o sistema.
    * 
    * @param quantidade a quantidade de contas teste para se criar
    * @param senha senha que as contas de teste terão
    */
   @SuppressWarnings( "all" )
   public static List< Conta > criarContasFicticia( final int quantidade,
            final String senha )
   {
      final ArrayList< Conta > contasTeste = new ArrayList<>();
      contasTeste.add( new Conta( "admin", "admin", 2000.5 * Biblioteca
               .gerarNumeroAleatorio(), true, new Inventario() ) );
      
      UtiliárioDeContas.criarInventarioFicticio( contasTeste.get( 0 ),
               quantidade );
      
      Conta contaTeste;
      
      for( int i = 0; i < quantidade; i++ )
      {
         contaTeste =
                  new Conta( "User" + Biblioteca.gerarNumeroAleatorio(), senha,
                           2000.5 * Biblioteca.gerarNumeroAleatorio(), false,
                           new Inventario() );
         UtiliárioDeContas.criarInventarioFicticio( contaTeste, quantidade );
         
         contasTeste.add( contaTeste );
      }
      if( UtiliárioDeContas.LOG.isLoggable( Level.SEVERE ) )
      {
         UtiliárioDeContas.LOG.severe( "Estou em criarContasFictícias "
                  + contasTeste.get( 0 ).getNome() );
      }
      return contasTeste;
   }
   
   /**
    * Cria um inventário fictício de ações contendo 5 ações fictícias.
    * 
    * @param conta a conta que irá receber as ações fictícioas.
    * @param quantidade a quantidade de ações fictícias para se criar.
    */
   @SuppressWarnings( {
      "all"
   } )
   public static void criarInventarioFicticio( final Conta conta,
            final int quantidade )
   {
      Ação ação;
      
      for( int i = 0; i < ( quantidade / 5 ); i++ )
      {
         ação =
                  new Ação( 2.2 + Biblioteca.gerarNumeroAleatorio(),
                           10 + Biblioteca.gerarNumeroAleatorio(),
                           "Tabajara SA" + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
                  new Ação( 22.2 + Biblioteca.gerarNumeroAleatorio(),
                           100 + Biblioteca.gerarNumeroAleatorio(),
                           "Tabajara SO" + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
                  new Ação( 200.2 + Biblioteca.gerarNumeroAleatorio(),
                           1000 + Biblioteca.gerarNumeroAleatorio(),
                           "Tabajara SP" + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
                  new Ação( 2000.2 + Biblioteca.gerarNumeroAleatorio(),
                           10000 + Biblioteca.gerarNumeroAleatorio(),
                           "Tabajara ST" + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
                  new Ação( 200006.2 + Biblioteca.gerarNumeroAleatorio(),
                           10000 + Biblioteca.gerarNumeroAleatorio(),
                           "Tabajara SS" + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
      }
   }
}
