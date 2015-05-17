package grafo;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;

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
   /**
    * Chama o motor de testes JUnit.
    */
   private static void executarTodosOs50Testes()
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
   
   public static void main( final String[] args )
            throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      Principal.testeCiclo();
      Principal.testeFechoString();
      Principal.testeFechoInt();
      Principal.testeGenéricoHashtable();
      Principal.testeDaBase();
      Principal.executarTodosOs50Testes();
   }
   
   /**
    * @throws ExeçãoVérticeNãoExistente
    * @throws ExeçãoVérticeJáExistente
    */
   @SuppressWarnings( "boxing" )
   private static void testeCiclo() throws ExeçãoVérticeNãoExistente,
            ExeçãoVérticeJáExistente
   {
      System.out.println( "\nTeste ciclo: " );
      final Grafo grafo = new Grafo();
      
      for( int i = 0; i < 6; i++ )
      {
         grafo.adicionaVértice( i );
      }
      grafo.conecta( 3, 2 );// ciclo
      grafo.conecta( 5, 2 );
      grafo.conecta( 3, 5 );// ciclo
      grafo.conecta( 1, 4 );
      
      System.out.println( "Grafo: " + grafo );
      System.out.println( "Há ciclos? " + grafo.háCiclos() );
   }
   
   private static void testeDaBase()
   {
      System.out.println( "\nTeste da base: " );
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
   
   /**
    * @throws ExeçãoVérticeJáExistente
    * @throws ExeçãoVérticeNãoExistente
    * 
    */
   @SuppressWarnings( "boxing" )
   private static void testeFechoInt() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      System.out.println( "\nTeste fecho transitivo com int: " );
      // Fecho transitivo
      final Grafo grafo = new Grafo();
      
      grafo.adicionaVértice( 0 );
      grafo.adicionaVértice( 1 );
      grafo.adicionaVértice( 2 );
      grafo.adicionaVértice( 3 );
      grafo.adicionaVértice( 4 );
      grafo.adicionaVértice( 5 );
      
      grafo.conecta( 1, 0 );
      grafo.conecta( 1, 2 );
      grafo.conecta( 2, 4 );
      grafo.conecta( 5, 3 );
      
      final Set< Object > fechoTransitivoTeste = grafo.fechoTransitivo( 4 );
      
      final Set< Object > fechoTransitivoModelo = grafo.adjacentes( 4 );
      fechoTransitivoModelo.add( 4 );
      fechoTransitivoModelo.add( 0 );
      fechoTransitivoModelo.add( 1 );
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      int i = 1;
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         System.out.println( i++ + " - Deve ser true: "
                  + fechoTransitivoTeste.contains( próximo ) );
      }
      System.out.println( "O tamanho deve ser 4: "
               + fechoTransitivoTeste.size() );
      System.out.println( "Grafo: " + grafo );
   }
   
   /**
    * @throws ExeçãoVérticeJáExistente
    * @throws ExeçãoVérticeNãoExistente
    */
   private static void testeFechoString() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      System.out.println( "\nTeste fecho transitivo com String: " );
      // Fecho transitivo
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      final Grafo grafo = new Grafo();
      
      grafo.adicionaVérticeArray( nomes );
      grafo.adicionaVértice( "João1" );
      grafo.adicionaVértice( "João2" );
      grafo.adicionaVértice( "João3" );
      grafo.adicionaVértice( "João4" );
      grafo.adicionaVértice( "João5" );
      grafo.adicionaVértice( "João6" );
      grafo.adicionaVértice( "João7" );
      grafo.adicionaVértice( "João8" );
      
      grafo.conectaArrayArray( nomes, nomes );
      grafo.conecta( "João1", nomes[0] );
      grafo.conecta( "João1", "João2" );
      grafo.conecta( "João2", "João3" );
      grafo.conecta( "João4", "João5" );
      grafo.conecta( "João5", "João6" );
      grafo.conecta( "João7", "João8" );
      grafo.conecta( "João6", "João7" );
      grafo.conecta( "João8", nomes[3] );
      
      final Set< Object > fechoTransitivoTeste =
               grafo.fechoTransitivo( "João4" );
      
      final Set< Object > fechoTransitivoModelo = grafo.adjacentes( "João4" );
      final Set< Object > temporário = grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( "João2" );
      fechoTransitivoModelo.add( "João3" );
      fechoTransitivoModelo.add( "João4" );
      fechoTransitivoModelo.add( "João6" );
      fechoTransitivoModelo.add( "João7" );
      fechoTransitivoModelo.add( "João8" );
      fechoTransitivoModelo.add( nomes[0] );
      fechoTransitivoModelo.addAll( temporário );
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      int i = 1;
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         System.out.println( i++ + " - Deve ser true: "
                  + fechoTransitivoTeste.contains( próximo ) );
      }
      System.out.println( "O tamanho deve ser 13: "
               + fechoTransitivoTeste.size() );
   }
   
   private static void testeGenéricoHashtable()
   {
      System.out.println( "\nTeste genérico da Hashtable: " );
      
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
