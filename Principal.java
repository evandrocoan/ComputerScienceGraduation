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
     * @param args
     * @throws ExeçãoElementoNãoEncontrado
     */
    public static void main( String[] args ) throws ExeçãoElementoNãoEncontrado
    {
        Principal.testeGenéricoHashtable();
        
        Principal.testeDeGrau();
        
        Principal.finalizarTestes();
    }
    
    private static void finalizarTestes()
    {
        System.out.println( Principal.falharam
            + " testes de " + Principal.total + " falharam!" );
        System.out.println( Principal.total
            - Principal.falharam + " testes de " + Principal.total
            + " foram bem sucedidos!" );
    }
    
    private static void testeGenéricoHashtable()
    {
        // cria uma table hash
        Hashtable< Integer, String > htable1 = new Hashtable<>();
        htable1.put( new Integer( 1 ), "A" );
        htable1.put( new Integer( 2 ), "B" );
        htable1.put( new Integer( 3 ), "C" );
        htable1.put( new Integer( 4 ), "D" );
        
        // cria uma Enumeration
        Enumeration< Integer > en = htable1.keys();
        
        System.out.printf( en.nextElement() + " " );
        System.out.printf( en.nextElement() + " " );
        System.out.printf( en.nextElement() + " " );
        System.out.printf( en.nextElement() + " \n" );
    }
    
    private static void testeDeGrau() throws ExeçãoElementoNãoEncontrado
    {
        String[] nomes = { "Brazil", "United States", "China", "Hong Kong" };
        Grafo grafo = new Grafo();
        boolean sucesso = false;
        
        try
        {
            grafo.adicionarVértices( nomes );
            grafo.adicionarVértice( "Franca" );
            grafo.conectarVértices( "Franca", nomes );
            
        } catch( ExeçãoVérticeJáExistente e1 )
        {
            e1.printStackTrace();
        }
        falharam++;
        total++;
        
        try
        {
            if( ( grafo.grauDoVértice( "Franca" ) == 4 ) )
            {
                sucesso = true;
                falharam--;
            } else
            {
                System.out.println( "Grau do vértice grau: "
                    + grafo.grauDoVértice( "Franca" ) );
            }
            
        } catch( ExeçãoElementoNãoEncontrado e )
        {
            e.printStackTrace();
        }
        System.out.println( grafo.toString() );
        System.out.println( "Teste de grau: " + ( sucesso? "OK!" : "Falhou!" ) );
    }
}
