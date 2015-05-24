package homebroker.testes;

/**
 * 
 * @author Evandro  Coan
 */
@SuppressWarnings( { "all" } )
public class BadClass
{
    /**
     * 
     */
    public void badMethod()
    {
        try
        {
            final String bad = "";
        } catch( final Exception e )
        {
        }
    }
}