/**
 * Pacote que contém a classe principal de testes.
 */
package testes.interfaces.gráficas;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author http://stackoverflow.com/users/3973/binil-thomas
 * @see <a
 *      href="http://stackoverflow.com/questions/6083963/is-it-possible-to-have-multiple-joptionpane-dialogs">
 *      is it possible to have multiple JOptionPane dialogs?</a>
 */
public class DialogTest
{
    static final SimpleDateFormat SDF = new SimpleDateFormat(
            "MM/dd/yyyy hh:mm:ss" );
    
    /**
     * @param args
     */
    public static void main( String[] args )
    {
        
        final JFrame frame = new JFrame( "Dialog test" );
        frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
        frame.add( createPanelToPopDialog( frame ) );
        frame.setSize( 500, 200 );
        frame.setVisible( true );
    }
    
    static JPanel createPanelToPopDialog( final JFrame parent )
    {
        JPanel panel = new JPanel();
        panel.setLayout( new BorderLayout() );
        JButton button = new JButton( "Pop a Dialog" );
        button.addActionListener( new ActionListener()
        {
            @Override
            public void actionPerformed(
                    @SuppressWarnings( "unused" ) ActionEvent e )
            {
                JDialog dialog = new JDialog( parent, true );
                dialog.add( createPanelToPopDialog( parent ) );
                dialog.setSize( 500, 200 );
                dialog.setVisible( true );
            }
        } );
        panel.add( button, BorderLayout.SOUTH );
        panel.add( new JLabel( "Created at " + SDF.format( new Date() ) ) );
        
        panel.setBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ) );
        return panel;
    }
}