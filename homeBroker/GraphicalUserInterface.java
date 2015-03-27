package homeBroker;
import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

/**
 * @author Professional
 *
 */
public class GraphicalUserInterface extends JFrame
{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private JPanel contentPane;
    
    /**
     * 
     */
    public GraphicalUserInterface()
    {
        setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
        setBounds( 100, 100, 450, 300 );
        this.contentPane = new JPanel();
        this.contentPane.setBorder( new EmptyBorder( 5, 5, 5, 5 ) );
        this.contentPane.setLayout( new BorderLayout( 0, 0 ) );
        setContentPane( this.contentPane );
        
    }
    
}
