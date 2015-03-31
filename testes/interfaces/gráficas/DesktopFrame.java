/**
 * Pacote que contém a classe principal de testes.
 */
package testes.interfaces.gráficas;

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
    private JDesktopPane theDesktop;
    
    /**
     * set up GUI
     */
    public DesktopFrame()
    {
        super( "Using a JDesktopPane" );
        
        JMenuBar bar = new JMenuBar(); // create menu bar
        JMenu addMenu = new JMenu( "Add" ); // create Add menu
        JMenuItem newFrame = new JMenuItem( "Internal Frame" );
        
        addMenu.add( newFrame ); // add new frame item to Add menu
        bar.add( addMenu ); // add Add menu to menu bar
        setJMenuBar( bar ); // set menu bar for this application
        
        this.theDesktop = new JDesktopPane(); // create desktop pane
        add( this.theDesktop ); // add desktop pane to frame
        
        // set up listener for newFrame menu item
        newFrame.addActionListener(
        
        new ActionListener() // anonymous inner class
        {
            // display new internal window
            @Override
            public void actionPerformed(
                    @SuppressWarnings( "unused" ) ActionEvent event )
            {
                // create internal frame
                JInternalFrame frame =
                        new JInternalFrame( "Internal Frame", true, true, true,
                                true );
                
                MyJPanel panel = new MyJPanel(); // create new panel
                frame.add( panel, BorderLayout.CENTER ); // add panel
                frame.pack(); // set internal frame to size of contents
                
                DesktopFrame.this.theDesktop.add( frame ); // attach internal
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
    private ImageIcon picture; // image to be displayed
    private String[] images = { "yellowflowers.png", "purpleflowers.png",
            "redflowers.png", "redflowers2.png", "lavenderflowers.png" };
    
    // load image
    public MyJPanel()
    {
        int randomNumber = generator.nextInt( 5 );
        this.picture = new ImageIcon( this.images[randomNumber] ); // set icon
    } // end MyJPanel constructor
    
    // display imageIcon on panel
    @Override
    public void paintComponent( Graphics g )
    {
        super.paintComponent( g );
        this.picture.paintIcon( this, g, 0, 0 ); // display icon
    } // end method paintComponent
    
    // return image dimensions
    @Override
    public Dimension getPreferredSize()
    {
        return new Dimension( this.picture.getIconWidth(),
                this.picture.getIconHeight() );
    } // end method getPreferredSize
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
