package homeBroker;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import javax.swing.border.EmptyBorder;

/**
 * @author Professional
 *
 */
public class GraphicalUserInterface extends JFrame
{
    private JPanel contentPane;
    
    /**
     * 
     */
    public GraphicalUserInterface()
    {
        this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
        this.setBounds( 100, 100, 450, 300 );
        this.contentPane = new JPanel();
        this.contentPane.setBorder( new EmptyBorder( 5, 5, 5, 5 ) );
        this.contentPane.setLayout( new BorderLayout( 0, 0 ) );
        this.setContentPane( this.contentPane );
    }
    
}
