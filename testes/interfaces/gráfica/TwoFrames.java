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
package testes.interfaces.gráfica;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

/**
 * @author http://stackoverflow.com/users/1057230/nice-cow
 * @see <a
 *      href="http://stackoverflow.com/questions/9443161/swing-multiple-screens-transfer-of-control">
 *      Swing , Multiple Screens , Transfer of control</a>
 */
public class TwoFrames
{
    // Making our first JFrame.
    JFrame frame1 = new JFrame( "FRAME 1" );
    // Declaring our second JFrame.
    JFrame frame2;
    
    /**
     * 
     */
    public void createAndDisplayGUI()
    {
        // Used to close the JFrame graciously.
        this.frame1.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
        
        // Used to position the JFrame at the middle of the screen.
        // frame1.setLocationRelativeTo(null);
        
        // Use this instead for placing windows, as determined by the OS.
        this.frame1.setLocationByPlatform( true );
        
        // Calling this method to create our frame2.
        this.makeNewFrame();
        
        // Button to show the second JFrame.
        JButton showButton = new JButton( "SHOW NEW FRAME" );
        showButton.addActionListener( new ActionListener()
        {
            @Override
            public void actionPerformed(
                    @SuppressWarnings( "unused" ) ActionEvent ae )
            {
                // Checking if the frame2 is already visible
                // on the screen, if YES then we won't
                // create a new frame, else a new frame
                // will be created. This will prevent multiple
                // JFrame to be created at the click of this button.
                if( !( TwoFrames.this.frame2.isShowing() ) )
                {
                    // If you had already disposed it previously
                    // by clicking the hide Button on the other frame
                    // then the click on this button will recreate
                    // a new frame to be displayed.
                    makeNewFrame();
                    TwoFrames.this.frame2.setVisible( true );
                }
            }
        } );
        
        // Adding the button to the South side of the frame1.
        this.frame1.add( showButton, BorderLayout.PAGE_END );
        this.frame1.pack();
        this.frame1.setVisible( true );
    }
    
    private void makeNewFrame()
    {
        this.frame2 = new JFrame( "FRAME 2" );
        this.frame2.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
        this.frame2.setLocationByPlatform( true );
        
        // Creating a JButton to be shown on the JFrame.
        JButton hideButton = new JButton( "HIDE FRAME" );
        hideButton.addActionListener( new ActionListener()
        {
            @SuppressWarnings( "unused" )
            @Override
            public void actionPerformed( ActionEvent ae )
            {
                // On the click of this button, frame2 will
                // disappear and HAI will be displayed on the console.
                TwoFrames.this.frame2.dispose();
                System.out.println( "HAI" );
            }
        } );
        
        // Adding the button to the South side of the frame1.
        this.frame2.add( hideButton, BorderLayout.PAGE_END );
        this.frame2.pack();
    }
    
    /**
     * @param args
     */
    public static void main( String... args )
    {
        /*
         * Here we are Secheduling a JOB for Event Dispatcher Thread, since
         * Swing is not Thread Safe. This is used to place the code which is
         * responsible for creating and diaplaying your GUI.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            @Override
            public void run()
            {
                TwoFrames tf = new TwoFrames();
                tf.createAndDisplayGUI();
            }
        } );
    }
}