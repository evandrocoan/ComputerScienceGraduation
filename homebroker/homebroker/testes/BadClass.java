package homebroker.testes;

/**
 * 
 * @author Professional
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