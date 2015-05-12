package grafo;

import java.util.Arrays;
import java.util.Collections;
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
    public void testAdicionarVértice() throws ExeçãoVérticeNãoExistente,
        ExeçãoVérticeJáExistente
    {
        GrafoTest.grafo.adicionaVértice( "Brasil" );
        Assert.assertEquals( 0, GrafoTest.grafo.grau( "Brasil" ) );
        
        GrafoTest.grafo.adicionaVértice( "China" );
        GrafoTest.grafo.conecta( "Brasil", "China" );
        Assert.assertEquals( 1, GrafoTest.grafo.grau( "Brasil" ) );
    }
    
    @Test
    public void testAdicionarVérticeConectadoÀObjectEnumeration()
        throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
    {
        final String[] nomes = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
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
        throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
    {
        final String[] nomes = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
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
    public void testAdjacentes() throws ExeçãoVérticeNãoExistente,
        ExeçãoVérticeJáExistente
    {
        final String[] nomesArray = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomesArray );
        GrafoTest.grafo.conecta( nomesArray[0], nomesArray );
        
        final Vector< String > nomesVetor =
            new Vector<>( Arrays.asList( nomesArray ) );
        final Iterator< String > nomesIterador = nomesVetor.iterator();
        
        final Enumeration< ? > adjacentes =
            GrafoTest.grafo.adjacentes( nomesArray[0] );
        final List< ? > adjacentesLista = Collections.list( adjacentes );
        
        while( adjacentes.hasMoreElements() )
        {
            final Object objeto = nomesIterador.next();
            Assert.assertTrue( adjacentesLista.contains( objeto ) );
        }
        Assert.assertTrue( 5 == GrafoTest.grafo.ordem() );
    }
    
    @Test
    public void testDesconectarVérticesObjectObject()
        throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
    {
        final String[] nomes = {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        final Vector< String > adjacentes = new Vector<>();
        adjacentes.add( nomes[1] );
        adjacentes.add( nomes[4] );
        
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
    public void testÉCompleto() throws ExeçãoVérticeJáExistente,
        ExeçãoVérticeNãoExistente
    {
        final String[] nomes = {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes, nomes );
        Assert.assertTrue( GrafoTest.grafo.éCompleto() );
        
        GrafoTest.grafo.desconecta( nomes[0], nomes[0] );
        GrafoTest.grafo.desconecta( nomes[1], nomes[1] );
        GrafoTest.grafo.desconecta( nomes[2], nomes[2] );
        GrafoTest.grafo.desconecta( nomes[3], nomes[3] );
        GrafoTest.grafo.desconecta( nomes[4], nomes[4] );
        Assert.assertTrue( GrafoTest.grafo.éCompleto() );
        
        GrafoTest.grafo.desconecta( nomes[0], nomes[1] );
        Assert.assertFalse( GrafoTest.grafo.éCompleto() );
        
        GrafoTest.grafo.conecta( nomes[0], nomes[0] );
        GrafoTest.grafo.conecta( nomes[1], nomes[1] );
        GrafoTest.grafo.conecta( nomes[2], nomes[2] );
        GrafoTest.grafo.conecta( nomes[3], nomes[3] );
        GrafoTest.grafo.conecta( nomes[4], nomes[4] );
        Assert.assertFalse( GrafoTest.grafo.éCompleto() );
        
        GrafoTest.grafo.conecta( nomes[0], nomes[1] );
        GrafoTest.grafo.desconecta( nomes[1], nomes[4] );
        Assert.assertFalse( GrafoTest.grafo.éCompleto() );
        
        GrafoTest.grafo.conecta( nomes[1], nomes[4] );
        Assert.assertTrue( GrafoTest.grafo.éCompleto() );
    }
    
    @Test
    public void testÉRegular() throws ExeçãoVérticeJáExistente,
    ExeçãoVérticeNãoExistente
    {
        final String[] nomesArray = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomesArray );
        Assert.assertTrue( GrafoTest.grafo.éRegular() );
        
        GrafoTest.grafo.conecta( nomesArray[0], nomesArray[1] );
        Assert.assertFalse( GrafoTest.grafo.éRegular() );
        
        GrafoTest.grafo.conecta( nomesArray[2], nomesArray[3] );
        Assert.assertFalse( GrafoTest.grafo.éRegular() );
        
        GrafoTest.grafo.adicionaVértice( "Turquia" );
        GrafoTest.grafo.conecta( nomesArray[4], "Turquia" );
        Assert.assertTrue( GrafoTest.grafo.éRegular() );
    }
    
    @Test
    public void testEstãoConectados() throws ExeçãoVérticeNãoExistente,
        ExeçãoVérticeJáExistente
    {
        final String[] nomes = {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes[0], nomes );
        Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
    }
    
    @Test
    public void testFechoTransitivo() throws ExeçãoVérticeJáExistente,
        ExeçãoVérticeNãoExistente
    {
        final String[] nomes = {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes, nomes );
        
        final Enumeration< ? > fechoTransitivo =
            GrafoTest.grafo.fechoTransitivo( nomes[0] );
    } // TODO
    
    @Test
    public void testRemoverVértice() throws ExeçãoVérticeNãoExistente,
        ExeçãoVérticeJáExistente
    {
        final String[] nomes = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomes );
        GrafoTest.grafo.conecta( nomes[0], nomes );
        Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
        
        GrafoTest.grafo.removerVértice( nomes[1] );
        Assert.assertEquals( 4, GrafoTest.grafo.grau( nomes[0] ) );
    }
    
    @Test
    public void testUmVértice() throws ExeçãoVérticeJáExistente
    {
        final String[] nomesArray = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomesArray );
        final Object umVértice = GrafoTest.grafo.umVértice();
        Assert.assertTrue( GrafoTest.grafo.contémVertice( umVértice ) );
    }
    
    @Test
    public void testUmVérticeObject() throws ExeçãoVérticeJáExistente,
        ExeçãoVérticeNãoExistente
    {
        final String[] nomesArray = new String[] {
            "Brasil", "USA", "China", "Hong Kong", "Japão"
        };
        GrafoTest.grafo.adicionaVértice( nomesArray );
        Object umVértice = GrafoTest.grafo.umVértice( nomesArray[0] );
        Assert.assertEquals( umVértice, nomesArray[0] );
        
        umVértice = GrafoTest.grafo.umVértice( nomesArray[2] );
        Assert.assertEquals( umVértice, nomesArray[2] );
        
        Exception exeção = null;
        try
        {
            GrafoTest.grafo.umVértice( "Cuba" );
            Assert.fail( "Exeção uma exeção deve ser lançada!" );
        } catch( final ExeçãoVérticeNãoExistente e )
        {
            exeção = e;
        }
        Assert.assertTrue( exeção instanceof ExeçãoVérticeNãoExistente );
    }
    
    @Test( expected = ExeçãoVérticeNãoExistente.class )
    public void testUmVérticeObjectExeption() throws ExeçãoVérticeNãoExistente
    {
        GrafoTest.grafo.umVértice( "Cuba" );
    }
}
