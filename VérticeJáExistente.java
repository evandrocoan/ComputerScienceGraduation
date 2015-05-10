/**
 * 
 * @author Professional
 */
public class VérticeJáExistente extends Exception
{
    private final Object vértice;
    private final Grafo grafo;
    
    /**
     * @param vértice o vértice do grafo.
     * @param grafo o objeto do grafo.
     */
    public VérticeJáExistente( final Object vértice, final Grafo grafo )
    {
        this.vértice = vértice;
        this.grafo = grafo;
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
