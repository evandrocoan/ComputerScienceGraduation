import java.util.Enumeration;
import java.util.Vector;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings( "javadoc" )
public class GrafoTest
{
    Grafo grafo;
    String[] nomes;
    
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
    public void testAdcionarVérticeConectadoÀObjectObjectArray()
            throws ExeçãoVérticeJáExistente, ExeçãoElementoNãoEncontrado
    {
        this.grafo.adicionarVértice( this.nomes );
        this.grafo.conectarVértice( "Brasil", this.nomes );
        
        Assert.assertEquals( 6, this.grafo.grauDoVértice( "Brasil" ) );
        
        this.grafo.adicionarVértice( "Tcheca" );
        this.grafo.conectarVértices( "Tcheca", "Brasil" );
        Assert.assertEquals( 7, this.grafo.grauDoVértice( "Brasil" ) );
        
        this.grafo.conectarVértices( "Tcheca", "Tcheca" );
        Assert.assertEquals( 3, this.grafo.grauDoVértice( "Tcheca" ) );
    }
    
    @Test
    public void testAdicionarVértice()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testAdicionarVérticeConectadoÀObjectEnumerationOfQ()
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
    }
    
    @Test
    public void testAdicionarVérticeObjectArray()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testConectarVérticesObjectObject()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testConectarVérticesObjectObjectArray()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testCriarUmGrafoCompleto()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testDesconectarVértices()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testEstãoConectados()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testGrafo()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testGrafoObjectArray()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testGrauDoVértice()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testHáAntiCliqueNesteGrafoDe()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testHáCliqueNesteGrafo()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testObterAdjacentes()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testObterAdjacentesEmArranjo()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testObterGrafoComplementar()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testObterSubGrafoDoVértice()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testObterVérticesDoGrafo()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testRemoverVértice()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testTamanhoDoGrafo()
    {
        Assert.fail( "Not yet implemented" );
    }
    
    @Test
    public void testToString()
    {
        Assert.fail( "Not yet implemented" );
    }
    
}
