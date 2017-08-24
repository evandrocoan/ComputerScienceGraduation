
import org.junit.Test;
import junit.framework.TestCase;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

public class HamcrestTests extends TestCase
{
    @Test
    public void testEqualsObjectMatcher()
    {
        String theBiscuit = new String( "Ginger" );
        String myBiscuit = new String( "Ginger" );

        assertThat( theBiscuit, equalTo( myBiscuit ) );
    }

    @Test
    public void testEqualsNumberMatcher()
    {
        int theBiscuit = 5;
        int myBiscuit = 4;

        assertThat( theBiscuit, greaterThan( myBiscuit ) );
    }

    @Test
    public void testEqualsTextMatcher()
    {
        String theBiscuit = new String( "First Part" );
        String myBiscuit = new String( "Part" );

        assertThat( theBiscuit, endsWith( myBiscuit ) );
    }

    @Test
    public void testEqualsArrayMatcher()
    {
        String myBiscuit = "myValue";
        String array[] = new String[] {
                    "foo", "bar"
        };

        assertThat( myBiscuit, allOf( startsWith( "my" ), containsString( "Val" ) ) );
    }
}
