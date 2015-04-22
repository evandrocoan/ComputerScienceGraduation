import java.util.Enumeration;
import java.util.Vector;

import org.junit.Assert;
import org.junit.Test;

@SuppressWarnings( "javadoc" )
public class GrafoTest
{
    @Test
    public void testAdcionarVérticeConectadoÀObjectObjectArray()
            throws ExeçãoVérticeJáExistente, ExeçãoElementoNãoEncontrado
    {
        final Grafo grafo = new Grafo();
        final String[] nomes =
                { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        
        grafo.adicionarVértice( nomes );
        grafo.conectarVértice( "Brasil", nomes );
        
        Assert.assertEquals( 5, grafo.grauDoVértice( "Brasil" ) );
        
        grafo.adicionarVértice( "Tcheca" );
        grafo.conectarVértices( "Tcheca", "Brasil" );
        Assert.assertEquals( 6, grafo.grauDoVértice( "Brasil" ) );
        
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
        final String[] nomes =
                { "Brasil", "USA", "China", "Hong Kong", "Japão" };
        final Grafo grafo = new Grafo();
        
        final Vector< String > adjacente = new Vector<>();
        adjacente.add( nomes[1] );
        adjacente.add( nomes[4] );
        final Enumeration< ? > adjacentes = adjacente.elements();
        
        grafo.adicionarVértice( nomes );
        grafo.adicionarVérticeConectadoÀ( "França", adjacentes );
        Assert.assertEquals( 2, grafo.grauDoVértice( "França" ) );
        
        grafo.adicionarVértice( "Tcheca" );
        grafo.conectarVértices( "Tcheca", "França" );
        Assert.assertEquals( 3, grafo.grauDoVértice( "França" ) );
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
