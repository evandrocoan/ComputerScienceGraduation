package grafo;

/**
 * 
 * @author Professional
 */
public class ElementoNãoEncontrado extends Exception
{
    private Object vértice;
    private final Grafo grafo;
    
    /**
     * @param vértice o vértice do grafo
     * @param grafo o objeto do grafo
     */
    public ElementoNãoEncontrado( final Object vértice, final Grafo grafo )
    {
        this.vértice = vértice;
        this.grafo = grafo;
    }
    
    /**
     * @param vértice the vértice to set
     */
    public void ajustarVértice( final Object vértice )
    {
        this.vértice = vértice;
    }
    
    /**
     * @return the grafo
     */
    public Grafo obterGrafo()
    {
        return this.grafo;
    }
    
    /**
     * @return the vértice
     */
    public Object obterVértice()
    {
        return this.vértice;
    }
    
}
