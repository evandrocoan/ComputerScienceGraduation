import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runners.MethodSorters;

@FixMethodOrder( MethodSorters.NAME_ASCENDING )
public class GrafoTest
{
    /**
     * Resposável por realizar o debug do programa.
     */
    static final Logger LOG = Logger.getLogger( GrafoTest.class.getName() );
    
    private static Grafo grafo;
    
    @BeforeClass
    public static void configureLOG()
    {
        GrafoTest.LOG.setLevel( Level.ALL );
    }
    
    @Rule
    public TestWatcher watchman = new TestWatcher()
    {
        @Override
        protected void starting( final Description description )
        {
            final String methodName = description.getMethodName();
            String className = description.getClassName();
            className = className.substring( className.lastIndexOf( '.' ) + 1 );
            final String message = className + " " + methodName;
            System.err.printf( "Starting JUnit-test: " + message + " " );
        }
    };
    
    @After
    public void logResults()
    {
        GrafoTest.LOG.info( GrafoTest.grafo.toString() + "\n " );
    }
    
    /**
     * É executado antes que um teste inicia. Os testes execuram na seguinte
     * ordem: setUp(), test1(), printBye(), setUp(), test2(), printBye()...
     */
    @Before
    public void setUp()
    {
        GrafoTest.grafo = new Grafo();
    }
    
    @Test
    public void testAdicionarVértice() throws ElementoNãoEncontrado,
        VérticeJáExistente
    {
        GrafoTest.grafo.adicionaVértice( "Brasil" );
        Assert.assertEquals( 0, GrafoTest.grafo.grau( "Brasil" ) );
        
        GrafoTest.grafo.adicionaVértice( "China" );
        GrafoTest.grafo.conecta( "Brasil", "China" );
        Assert.assertEquals( 1, GrafoTest.grafo.grau( "Brasil" ) );
    }
    
    @Test
    public void testAdicionarVérticeConectadoÀObjectEnumeration()
        throws VérticeJáExistente, ElementoNãoEncontrado
    {
        final String[] nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        final Vector< String > adjacentesVector = new Vector<>();
        adjacentesVector.add( nomes[1] );
        adjacentesVector.add( nomes[4] );
        final Enumeration< ? > adjacentes = adjacentesVector.elements();
        
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.adicionaVértice( "França", adjacentes );
        Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
        
        GrafoTest.grafo.adicionaVértice( "Tcheca" );
        GrafoTest.grafo.conecta( "Tcheca", "França" );
        Assert.assertEquals( 3, GrafoTest.grafo.grau( "França" ) );
    }
    
    @Test
    public void testAdicionarVérticeConectadoÀObjectObjectArray()
        throws VérticeJáExistente, ElementoNãoEncontrado
    {
        final String[] nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes[0], nomes );
        Assert.assertEquals( 5, GrafoTest.grafo.grau( nomes[0] ) );
        
        GrafoTest.grafo.adicionaVértice( "Tcheca" );
        GrafoTest.grafo.conecta( "Tcheca", nomes[0] );
        Assert.assertEquals( 6, GrafoTest.grafo.grau( nomes[0] ) );
        
        GrafoTest.grafo.conecta( "Tcheca", "Tcheca" );
        Assert.assertEquals( 2, GrafoTest.grafo.grau( "Tcheca" ) );
    }
    
    @Test
    public void testAdjacentes() throws ElementoNãoEncontrado,
        VérticeJáExistente
    {
        final String[] nomesArray =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        
        final Vector< String > nomesVetor =
            new Vector<>( Arrays.asList( nomesArray ) );
        
        final Iterator< String > nomesIterador = nomesVetor.iterator();
        final Enumeration< ? > nomesEnumeration = nomesVetor.elements();
        
        GrafoTest.grafo.adicionaVértice( nomesEnumeration );
        GrafoTest.grafo.conecta( nomesArray[0], nomesEnumeration );
        
        final Enumeration< ? > adjacentes =
            GrafoTest.grafo.adjacentes( nomesArray[0] );
        
        final List< ? > adjacentesConjunto = new ArrayList<>( nomesVetor );
        
        while( adjacentes.hasMoreElements() )
        {
            final Object objeto = nomesIterador.next();
            Assert.assertTrue( adjacentesConjunto.contains( objeto ) );
        }
        Assert.assertTrue( 5 == GrafoTest.grafo.ordem() );
    }
    
    @Test
    public void testDesconectarVérticesObjectObject()
        throws VérticeJáExistente, ElementoNãoEncontrado
    {
        final String[] nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        final Vector< String > adjacente = new Vector<>();
        adjacente.add( nomes[1] );
        adjacente.add( nomes[4] );
        final Enumeration< ? > adjacentes = adjacente.elements();
        
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.adicionaVértice( "França", adjacentes );
        Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
        
        GrafoTest.grafo.adicionaVértice( "Tcheca" );
        GrafoTest.grafo.conecta( "Tcheca", "França" );
        Assert.assertEquals( 3, GrafoTest.grafo.grau( "França" ) );
        
        GrafoTest.grafo.desconecta( "Tcheca", "França" );
        Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
        Assert.assertEquals( 0, GrafoTest.grafo.grau( "Tcheca" ) );
    }
    
    @Test
    public void testEstãoConectados() throws ElementoNãoEncontrado,
        VérticeJáExistente
    {
        final String[] nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes[0], nomes );
        Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
    }
    
    @Test
    public void testRemoverVértice() throws ElementoNãoEncontrado,
        VérticeJáExistente
    {
        final String[] nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes[0], nomes );
        Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
        
        GrafoTest.grafo.removerVértice( nomes[1] );
        Assert.assertEquals( 4, GrafoTest.grafo.grau( nomes[0] ) );
    }
    
    @Test
    public void testUmVértice() throws VérticeJáExistente
    {
        final String[] nomesArray =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        
        GrafoTest.grafo.adicionaVértice( nomesArray );
        
        final Object umVértice = GrafoTest.grafo.umVértice();
        
        Assert.assertTrue( GrafoTest.grafo.contémVertice( umVértice ) );
    }
}
