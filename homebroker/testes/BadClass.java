package homebroker.testes;

/**
 * 
 * @author Professional
 */
public class BadClass
{
    /**
     * 
     */
    public void badMethod()
    {
        try
        {
            @SuppressWarnings( "unused" )
            String bad = "";
        } catch( Exception e )
        {
            // TODO
        }
    }
}