import java.util.Enumeration;
import java.util.Vector;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class GrafoTest
{
    Grafo grafo;
    String[] nomes;
    
    @Test
    public void adcionarVérticeConectadoÀObjectObjectArrayTest()
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
