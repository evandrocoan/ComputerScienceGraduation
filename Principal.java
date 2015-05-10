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
    /**
     * Retorna uma representação em string da enumeração. Esta enumeração deve
     * ter um método toString que gere valores que possam significativamente
     * representar a string.
     * 
     * @param enumeração uma enumeração.
     * @return string uma string representado a enumeração
     */
    public static String enumeraçãoToString( final Enumeration< ? > enumeração )
    {
        String string = "";
        
        while( enumeração.hasMoreElements() )
        {
            final Object elemento = enumeração.nextElement();
            string += elemento + " ";
        }
        return string;
    }
    
    public static void main( final String[] args )
    {
        Principal.testeGenéricoHashtable();
        Principal.testeDaBase();
        
        // org.junit.runner.JUnitCore.main( "GrafoTest" );
        final Result result = JUnitCore.runClasses( GrafoTest.class );
        for( final Failure failure: result.getFailures() )
        {
            System.out.println( "Teste falhou: " + failure.getDescription() );
        }
        System.out.println( "\nTodos os testes funcinaram? "
            + ( result.wasSuccessful()? "Sim!" : "NÃO!!!!!!!!!!!!!!!!!!!" ) );
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
        System.out.println( Principal.enumeraçãoToString( arestasDoVértice1
            .elements() )
            + "(deve ter impresso o vértice 2)" );
        
        System.out.println( Principal.enumeraçãoToString( arestasDoVértice2
            .elements() )
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
}
