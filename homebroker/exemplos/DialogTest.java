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
        frame.add( DialogTest.createPanelToPopDialog( frame ) );
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
                dialog.add( DialogTest.createPanelToPopDialog( parent ) );
                dialog.setSize( 500, 200 );
                dialog.setVisible( true );
            }
        } );
        panel.add( button, BorderLayout.SOUTH );
        panel.add( new JLabel( "Created at "
            + DialogTest.SDF.format( new Date() ) ) );
        
        panel.setBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ) );
        return panel;
    }
}