package grafo;

import java.util.Enumeration;
import java.util.Hashtable;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

/**
 * Testa o Grafo. Sobre esta estrutura, implementar algum algoritmo (de busca,
 * de solução de algum problema, etc) que a teste.
 * 
 * @author Professional
 */
public class Principal
{
   @SuppressWarnings( "boxing" )
   public static void main( final String[] args )
            throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      Principal.testeGenéricoHashtable();
      Principal.testeDaBase();
      Principal.testes();
      
      final Grafo grafo = new Grafo();
      
      for( int i = 0; i < 25; i++ )
      {
         grafo.adicionaVértice( i );
      }
      grafo.conecta( 5, 20 );// ciclo
      grafo.conecta( 5, 11 );
      grafo.conecta( 20, 11 );// ciclo
      grafo.conecta( 3, 2 );
      grafo.conecta( 22, 18 );
      grafo.conecta( 15, 14 );
      grafo.conecta( 13, 8 );
      grafo.conecta( 14, 4 );
      grafo.conecta( 21, 24 );
      grafo.conecta( 19, 24 );
      grafo.conecta( 17, 9 );
      grafo.conecta( 16, 17 );
      grafo.conecta( 17, 19 );
      
      System.out.println( "Grafo: " + grafo );
      
      System.out.println( "Há ciclos? " + grafo.háCiclos() );
   }
   
   private static void testeDaBase()
   {
      final Hashtable< Object, Hashtable< Object, Object >> vértices =
               new Hashtable<>();
      
      // cria dois vértices
      final String vértice1 = new String( "Este é o vértice 1" );
      final String vértice2 = new String( "Este é o vértice 2" );
      
      // pega a chave do vértice
      final Integer chaveDoVértice1 = Integer.valueOf( vértice1.hashCode() );
      final Integer chaveDoVértice2 = Integer.valueOf( vértice2.hashCode() );
      
      // adiciona o vértice na Hashtable de vértices
      vértices.put( vértice1, new Hashtable<>() );
      vértices.put( vértice2, new Hashtable<>() );
      
      // pega a Hashtable de arestas do vértice
      final Hashtable< Object, Object > arestasDoVértice1 =
               vértices.get( vértice1 );
      final Hashtable< Object, Object > arestasDoVértice2 =
               vértices.get( vértice2 );
      
      // conecta o vértice1 com o vértice2
      arestasDoVértice1.put( chaveDoVértice2, vértice2 );
      arestasDoVértice2.put( chaveDoVértice1, vértice1 );
      
      // imprime o resultado
      System.out.println( Principal.toString( arestasDoVértice1.elements() )
               + "(deve ter impresso o vértice 2)" );
      
      System.out.println( Principal.toString( arestasDoVértice2.elements() )
               + "(deve ter impresso o vértice 1)\n" );
   }
   
   private static void testeGenéricoHashtable()
   {
      // cria uma table hash
      final Hashtable< Integer, String > minhaHashtable = new Hashtable<>();
      minhaHashtable.put( new Integer( 1 ), "A" );
      minhaHashtable.put( new Integer( 2 ), "B" );
      minhaHashtable.put( new Integer( 3 ), "C" );
      minhaHashtable.put( new Integer( 4 ), "D" );
      
      System.out.printf( minhaHashtable.get( new Integer( 2 ) ) + " " );
      
      // cria uma Enumeration
      final Enumeration< Integer > minhaEnumeração = minhaHashtable.keys();
      
      System.out.printf( minhaEnumeração.nextElement() + " " );
      System.out.printf( minhaEnumeração.nextElement() + " " );
      System.out.printf( minhaEnumeração.nextElement() + " " );
      System.out.printf( minhaEnumeração.nextElement() + " \n" );
   }
   
   /** 
     * 
     */
   private static void testes()
   {
      final Result result = JUnitCore.runClasses( GrafoTest.class );
      final StringBuilder mensagem = new StringBuilder();
      if( result.getFailureCount() > 0 )
      {
         mensagem.append( "############## OS SEGUINTES TESTES FALHARAM!! "
                  + "#####################################\n" );
      } else
      {
         mensagem.append( "############## TODOS OS TESTES FORAM EXECUTADOS "
                  + "COM SUCESSO!! #######################\n" );
      }
      
      for( final Failure failure: result.getFailures() )
      {
         mensagem.append( failure.getDescription() ).append( '\n' );
         mensagem.append( failure.getMessage() ).append( '\n' );
      }
      System.out.println( mensagem );
   }
   
   /**
    * Retorna uma representação em string da enumeração. Esta enumeração deve
    * ter um método toString que gere valores que possam significativamente
    * representar a string.
    * 
    * @param enumeração uma enumeração.
    * @return string uma string representado a enumeração
    */
   public static String toString( final Enumeration< ? > enumeração )
   {
      String string = "";
      
      while( enumeração.hasMoreElements() )
      {
         final Object elemento = enumeração.nextElement();
         string += elemento + " ";
      }
      return string;
   }
   
   private Principal()
   {
   }
}
