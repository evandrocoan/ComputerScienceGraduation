import java.util.Enumeration;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class GrafoTest
{
    /**
     * Resposável por realizar o debug do programa.
     */
    private static final Logger LOG = Logger.getLogger( GrafoTest.class
        .getName() );
    
    @BeforeClass
    public static void configureLOG()
    {
        GrafoTest.LOG.setLevel( Level.ALL );
    }
    
    Grafo grafo;
    
    String[] nomes;

    @Test
    public void desconectarVérticesObjectObjectTest()
    {
        
    }
    
    /**
     * É executado antes que um teste inicia. Os testes execuram na seguinte
     * ordem: setUp(), test1(), printBye(), setUp(), test2(), printBye()...
     */
    @Before
    public void setUp()
    {
        this.nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        
        this.grafo = new Grafo();
    }
    
    @Test
    public void adcionarVérticeConectadoÀObjectObjectArrayTest()
        throws ExeçãoVérticeJáExistente, ExeçãoElementoNãoEncontrado
    {
        this.grafo.adicionarVértice( this.nomes );
        this.grafo.conectarVértices( "Brasil", this.nomes );
        
        Assert.assertEquals( 6, this.grafo.grauDoVértice( "Brasil" ) );
        
        this.grafo.adicionarVértice( "Tcheca" );
        this.grafo.conectarVértices( "Tcheca", "Brasil" );
        Assert.assertEquals( 7, this.grafo.grauDoVértice( "Brasil" ) );
        
        this.grafo.conectarVértices( "Tcheca", "Tcheca" );
        Assert.assertEquals( 3, this.grafo.grauDoVértice( "Tcheca" ) );
        
        GrafoTest.LOG.info( this.grafo.toString() + "\n " );
    }
    
    @Test
    public void testAdicionarVértice() throws ExeçãoElementoNãoEncontrado,
        ExeçãoVérticeJáExistente
    {
        this.grafo.adicionarVértice( "Brasil" );
        Assert.assertEquals( 0, this.grafo.grauDoVértice( "Brasil" ) );
        
        this.grafo.adicionarVértice( "China" );
        this.grafo.conectarVértices( "Brasil", "China" );
        Assert.assertEquals( 1, this.grafo.grauDoVértice( "Brasil" ) );
        
        GrafoTest.LOG.info( this.grafo.toString() + "\n " );
    }
    
    @Test
    public void adicionarVérticeConectadoÀObjectEnumerationTest()
        throws ExeçãoVérticeJáExistente, ExeçãoElementoNãoEncontrado
    {
        final Vector< String > adjacente = new Vector<>();
        adjacente.add( this.nomes[1] );
        adjacente.add( this.nomes[4] );
        final Enumeration< ? > adjacentes = adjacente.elements();
        
        this.grafo.adicionarVértice( this.nomes );
        this.grafo.adicionarVérticeConectadoÀ( "França", adjacentes );
        Assert.assertEquals( 2, this.grafo.grauDoVértice( "França" ) );
        
        this.grafo.adicionarVértice( "Tcheca" );
        this.grafo.conectarVértices( "Tcheca", "França" );
        Assert.assertEquals( 3, this.grafo.grauDoVértice( "França" ) );
        
        GrafoTest.LOG.info( this.grafo.toString() + "\n " );
    }
    
    /**
     * É executado antes que um teste inicia. Os testes execuram na seguinte
     * ordem: setUp(), test1(), printBye(), setUp(), test2(), printBye()...
     */
    @Before
    public void setUp()
    {
        this.nomes =
            new String[] { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        
        this.grafo = new Grafo();
    }
}
