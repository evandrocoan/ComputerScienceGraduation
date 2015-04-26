/**
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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;

import javax.swing.ImageIcon;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

/**
 * Fig. 22.11: DesktopFrame.java // Demonstrating JDesktopPane.
 * 
 * @author Deitel & Associates
 */
public class DesktopFrame extends JFrame
{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private final JDesktopPane theDesktop;
    
    /**
     * set up GUI
     */
    public DesktopFrame()
    {
        super( "Using a JDesktopPane" );
        
        final JMenuBar bar = new JMenuBar(); // create menu bar
        final JMenu addMenu = new JMenu( "Add" ); // create Add menu
        final JMenuItem newFrame = new JMenuItem( "Internal Frame" );
        
        addMenu.add( newFrame ); // add new frame item to Add menu
        bar.add( addMenu ); // add Add menu to menu bar
        this.setJMenuBar( bar ); // set menu bar for this application
        
        this.theDesktop = new JDesktopPane(); // create desktop pane
        this.add( this.theDesktop ); // add desktop pane to frame
        
        // set up listener for newFrame menu item
        newFrame.addActionListener(
                
                new ActionListener() // anonymous inner class
        {
            // display new internal window
            @SuppressWarnings( "synthetic-access" )
            @Override
            public void actionPerformed( final ActionEvent event )
            {
                // create internal frame
                final JInternalFrame frame =
                                new JInternalFrame( "Internal Frame", true, true, true,
                                true );
                
                final MyJPanel panel = new MyJPanel(); // create new panel
                frame.add( panel, BorderLayout.CENTER ); // add panel
                frame.pack(); // set internal frame to size of contents
                
                DesktopFrame.this.theDesktop.add( frame ); // attach
                // internal
                // frame
                frame.setVisible( true ); // show internal frame
            } // end method actionPerformed
        } // end anonymous inner class
        ); // end call to addActionListener
    } // end constructor DesktopFrame
} // end class DesktopFrame

// class to display an ImageIcon on a panel
class MyJPanel extends JPanel
{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static Random generator = new Random();
    private final ImageIcon picture; // image to be displayed
    private final String[] images = { "yellowflowers.png", "purpleflowers.png",
            "redflowers.png", "redflowers2.png", "lavenderflowers.png" };
    
    // load image
    public MyJPanel()
    {
        final int randomNumber = MyJPanel.generator.nextInt( 5 );
        this.picture = new ImageIcon( this.images[randomNumber] ); // set icon
    } // end MyJPanel constructor
    
    // return image dimensions
    @Override
    public Dimension getPreferredSize()
    {
        return new Dimension( this.picture.getIconWidth(),
                this.picture.getIconHeight() );
    } // end method getPreferredSize
    
    // display imageIcon on panel
    @Override
    public void paintComponent( final Graphics g )
    // 20/04/15 02:27
    {
        super.paintComponent( g );
        this.picture.paintIcon( this, g, 0, 0 ); // display icon
    } // end method paintComponent
} // end class MyJPanel

/**************************************************************************
 * (C) Copyright 1992-2005 by Deitel & Associates, Inc. and * Pearson Education,
 * Inc. All Rights Reserved. * * DISCLAIMER: The authors and publisher of this
 * book have used their * best efforts in preparing the book. These efforts
 * include the * development, research, and testing of the theories and programs
 * * to determine their effectiveness. The authors and publisher make * no
 * warranty of any kind, expressed or implied, with regard to these * programs
 * or to the documentation contained in these books. The authors * and publisher
 * shall not be liable in any event for incidental or * consequential damages in
 * connection with, or arising out of, the * furnishing, performance, or use of
 * these programs. *
 *************************************************************************/
