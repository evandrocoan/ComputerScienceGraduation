/**
 * Testa o Grapo. Sobre esta estrutura, implementar algum algoritmo (de busca,
 * de solução de algum problema, etc) que a teste.
 * 
 * @author Professional
 */
public class Principal
{
    /**
     * @param args
     * @throws Exception
     */
    public static void main( String[] args ) throws Exception
    {
        String[] suits = { "Clubs", "Diamonds", "Hearts", "Spades" };
        Grafo grafo = new Grafo( suits );
        grafo.conectar( "Clubs", "Diamonds" );
        grafo.conectar( "Clubs", "Hearts" );
        grafo.conectar( "Clubs", "Spades" );
        System.out.println( grafo.grau( "Clubs" ) + "" );
    }
}
