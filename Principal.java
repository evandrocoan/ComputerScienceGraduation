import java.util.Enumeration;
import java.util.Hashtable;

/**
 * Testa o Grafo. Sobre esta estrutura, implementar algum algoritmo (de busca,
 * de solução de algum problema, etc) que a teste.
 * 
 * @author Professional
 */
public class Principal
{
    private static int falharam = 0;
    private static int total = 0;
    
    /**
     * Retorna uma representação em string da enumeração. Está enumeração deve
     * ter um método toString que gere valores que possam significativamente
     * representar a string.
     * 
     * @param enumeração
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
    
    /**
     * @param args
     * @throws ExeçãoElementoNãoEncontrado
     */
    public static void main( final String[] args )
            throws ExeçãoElementoNãoEncontrado
    {
        Principal.testeGenéricoHashtable();
        
        Principal.testeDeGrau();
        
        Principal.finalizarTestes();
        
        Principal.testeDaBase();
        
        org.junit.runner.JUnitCore.main( "GrafoTest" );
    }
    
    private static void finalizarTestes()
    {
        System.out.println( Principal.falharam + " testes de "
                + Principal.total + " falharam!" );
        System.out.println( ( Principal.total - Principal.falharam )
                + " testes de " + Principal.total + " foram bem sucedidos!" );
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
                .elements() ) + "(deve ter impresso o vértice 2)" );
        
        System.out.println( Principal.enumeraçãoToString( arestasDoVértice2
                .elements() ) + "(deve ter impresso o vértice 1)" );
    }
    
    private static void testeDeGrau() throws ExeçãoElementoNãoEncontrado
    {
        final String[] nomes =
                { "Brazil", "United States", "China", "Hong Kong" };
        final Grafo grafo = new Grafo();
        boolean sucesso = false;
        
        try
        {
            grafo.adicionarVértice( nomes );
            grafo.adicionarVértice( "Franca" );
            grafo.conectarVértice( "Franca", nomes );
            grafo.conectarVértice( "Brazil", nomes );
            
        } catch( final ExeçãoVérticeJáExistente e1 )
        {
            e1.printStackTrace();
        }
        Principal.falharam++;
        Principal.total++;
        
        try
        {
            if( ( grafo.grauDoVértice( "Franca" ) == 4 )
                    & ( grafo.grauDoVértice( "Brazil" ) == 5 ) )
            {
                sucesso = true;
                Principal.falharam--;
            } else
            {
                System.out.println( "Grau do vértice Franca é: "
                        + grafo.grauDoVértice( "Franca" ) );
                System.out.println( "Mas deveria ser: 4" );
                System.out.println();
                
                System.out.println( "Grau do vértice Brasil é: "
                        + grafo.grauDoVértice( "Brazil" ) );
                System.out.println( "Mas deveria ser: 5" );
                System.out.println();
                
                System.out.println( grafo.toString() );
            }
        } catch( final ExeçãoElementoNãoEncontrado e )
        {
            e.printStackTrace();
        }
        System.out.println( "Teste de grau: " + ( sucesso? "OK!" : "Falhou!" ) );
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
