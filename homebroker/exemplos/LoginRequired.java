/**
 * Pacote que contém a classe principal de testes.
 * 
 * Use the plural for packages with homogeneous contents and the singular for
 * packages with heterogeneous contents.
 * 
 * A class is similar to a database relation. A database relation should be
 * named in the singular as its records are considered to be instances of the
 * relation. The function of a relation is to compose a complex record from
 * simple data.
 * 
 * A package, on the other hand, is not a data abstraction. It assists with
 * organization of code and resolution of naming conflicts. If a package is
 * named in the singular, it doesn't mean that each member of the package is an
 * instance of the package; it contains related but heterogeneous concepts. If
 * it is named in the plural (as they often are), I would expect that the
 * package contains homogeneous concepts.
 * 
 * For example, a type should be named "TaskCollection" instead of
 * "TasksCollection," as it is a collection containing instances of a Task. A
 * package named com.myproject.task does not mean that each contained class is
 * an instance of a task. There might be a TaskHandler, a TaskFactory, etc. A
 * package named com.myproject.tasks, however, would contain different types
 * that are all tasks: TakeOutGarbageTask, DoTheDishesTask, etc.
 * 
 * @author http://programmers.stackexchange.com/users/20202/matthew-rodatus
 * @see <a
 *      href="http://programmers.stackexchange.com/questions/75919/should-package-names-be-singular-or-plural">
 *      Should package names be singular or plural?</a>
 */
package homebroker.exemplos;

import java.awt.BorderLayout;
import java.awt.GridLayout;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

/**
 * @author http://stackoverflow.com/users/418556/andrew-thompson
 * @see <a
 *      href="http://stackoverflow.com/questions/10773132/how-to-unfocus-a-jtextfield">How
 *      to UnFocus a JTextField</a>
 */
public class LoginRequired
{
    
    LoginRequired()
    {
        JFrame f = new JFrame( "Login Required" );
        f.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
        
        f.setSize( 400, 300 );
        f.setResizable( false );
        f.setLocationByPlatform( true );
        f.setVisible( true );
        
        showLogin( f );
    }
    
    private static void showLogin( JFrame frame )
    {
        JPanel p = new JPanel( new BorderLayout( 5, 5 ) );
        
        JPanel labels = new JPanel( new GridLayout( 0, 1, 2, 2 ) );
        labels.add( new JLabel( "User Name", SwingConstants.RIGHT ) );
        labels.add( new JLabel( "Password", SwingConstants.RIGHT ) );
        p.add( labels, BorderLayout.WEST );
        
        JPanel controls = new JPanel( new GridLayout( 0, 1, 2, 2 ) );
        JTextField username = new JTextField( "Joe Blogs" );
        controls.add( username );
        JPasswordField password = new JPasswordField();
        password.addAncestorListener( new RequestFocusListener( false ) );
        controls.add( password );
        p.add( controls, BorderLayout.CENTER );
        
        // LayoutManager l = new GroupLayout(p);
        // p.setLayout(l);
        JOptionPane.showMessageDialog( frame, p, "Log In",
                JOptionPane.QUESTION_MESSAGE );
    }
    
    /**
     * @param args none
     */
    public static void main( String[] args )
    {
        SwingUtilities.invokeLater( new Runnable()
        {
            @SuppressWarnings( "unused" )
            @Override
            public void run()
            {
                new LoginRequired();
            }
        } );
    }
    
}

/**
 * Convenience class to request focus on a component.
 *
 * When the component is added to a realized Window then component will request
 * focus immediately, since the ancestorAdded event is fired immediately.
 *
 * When the component is added to a non realized Window, then the focus request
 * will be made once the window is realized, since the ancestorAdded event will
 * not be fired until then.
 *
 * Using the default constructor will cause the listener to be removed from the
 * component once the AncestorEvent is generated. A second constructor allows
 * you to specify a boolean value of false to prevent the AncestorListener from
 * being removed when the event is generated. This will allow you to reuse the
 * listener each time the event is generated.
 */
class RequestFocusListener implements AncestorListener
{
    private boolean removeListener;
    
    /*
     * Convenience constructor. The listener is only used once and then it is
     * removed from the component.
     */
    public RequestFocusListener()
    {
        this( true );
    }
    
    /*
     * Constructor that controls whether this listen can be used once or
     * multiple times.
     * 
     * @param removeListener when true this listener is only invoked once
     * otherwise it can be invoked multiple times.
     */
    public RequestFocusListener( boolean removeListener )
    {
        this.removeListener = removeListener;
    }
    
    @Override
    public void ancestorAdded( AncestorEvent e )
    {
        JComponent component = e.getComponent();
        component.requestFocusInWindow();
        
        if( this.removeListener )
            component.removeAncestorListener( this );
    }
    
    @SuppressWarnings( "unused" )
    @Override
    public void ancestorMoved( AncestorEvent e )
    {
        // TODO
    }
    
    @SuppressWarnings( "unused" )
    @Override
    public void ancestorRemoved( AncestorEvent e )
    {
        // TODO
    }
}
