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

import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;

/**
 * @author http://stackoverflow.com/users/714968/mkorbel
 * @see <a
 *      href="http://stackoverflow.com/questions/10315536/mouse-wheel-listener-not-working-in-jscrollbar/10315718">
 *      mouse wheel listener not working in jscrollbar</a>
 */
public class ListPanel extends JFrame
{
    
    class PanelRenderer implements ListCellRenderer< Object >
    {
        
        @Override
        public Component getListCellRendererComponent( final JList< ? > list,
                final Object value, final int index, final boolean isSelected,
                final boolean cellHasFocus )
        {
            final JButton renderer = (JButton) value;
            renderer.setBackground( isSelected? Color.red : list
                    .getBackground() );
            return renderer;
        }
    }
    
    private static final long serialVersionUID = 1L;
    
    /**
     * @param text
     * @return
     */
    @SuppressWarnings( "javadoc" )
    public static JButton createButtons( final String text )
    {
        final JButton button = new JButton( text );
        return button;
    }
    
    /**
     * @return
     */
    @SuppressWarnings( "javadoc" )
    public static JPanel createPanel()
    {
        final JPanel panel = new JPanel();
        panel.setLayout( new GridLayout( 0, 1, 1, 1 ) );
        panel.add( ListPanel.createButtons( "one" ) );
        panel.add( ListPanel.createButtons( "two" ) );
        panel.add( ListPanel.createButtons( "three" ) );
        panel.add( ListPanel.createButtons( "four" ) );
        panel.add( ListPanel.createButtons( "five" ) );
        panel.add( ListPanel.createButtons( "six" ) );
        panel.add( ListPanel.createButtons( "seven" ) );
        panel.add( ListPanel.createButtons( "eight" ) );
        panel.add( ListPanel.createButtons( "nine" ) );
        panel.add( ListPanel.createButtons( "ten" ) );
        panel.add( ListPanel.createButtons( "eleven" ) );
        panel.add( ListPanel.createButtons( "twelwe" ) );
        return panel;
    }
    
    /**
     * @param args
     */
    @SuppressWarnings( "javadoc" )
    public static void main( final String[] args )
    {
        SwingUtilities.invokeLater( new Runnable()
        {
            
            @Override
            public void run()
            {
                final ListPanel frame = new ListPanel();
                frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
                // frame.pack();
                frame.setSize( 270, 200 );
                frame.setVisible( true );
            }
        } );
    }
    
    /**
     * 
     */
    public ListPanel()
    {
        this.setLayout( new GridLayout( 0, 2, 10, 10 ) );
        final DefaultListModel< JButton > model = new DefaultListModel<>();
        model.addElement( ListPanel.createButtons( "one" ) );
        model.addElement( ListPanel.createButtons( "two" ) );
        model.addElement( ListPanel.createButtons( "three" ) );
        model.addElement( ListPanel.createButtons( "four" ) );
        model.addElement( ListPanel.createButtons( "five" ) );
        model.addElement( ListPanel.createButtons( "six" ) );
        model.addElement( ListPanel.createButtons( "seven" ) );
        model.addElement( ListPanel.createButtons( "eight" ) );
        model.addElement( ListPanel.createButtons( "nine" ) );
        model.addElement( ListPanel.createButtons( "ten" ) );
        model.addElement( ListPanel.createButtons( "eleven" ) );
        model.addElement( ListPanel.createButtons( "twelwe" ) );
        final JList< JButton > list = new JList<>( model );
        list.setCellRenderer( new PanelRenderer() );
        final JScrollPane scroll1 = new JScrollPane( list );
        final JScrollBar scrollBar = scroll1.getVerticalScrollBar();
        scrollBar.addAdjustmentListener( new AdjustmentListener()
        {
            
            @Override
            public void adjustmentValueChanged( final AdjustmentEvent e )
            {
                System.out.println( "JScrollBar's current value = "
                        + scrollBar.getValue() );
            }
        } );
        this.add( scroll1 );
        final JScrollPane scroll2 = new JScrollPane( ListPanel.createPanel() );
        this.add( scroll2 );
        final JScrollBar scrollBar1 = scroll2.getVerticalScrollBar();
        scrollBar1.addAdjustmentListener( new AdjustmentListener()
        {
            
            @Override
            public void adjustmentValueChanged( final AdjustmentEvent e )
            {
                System.out.println( "JScrollBar's current value = "
                        + scrollBar1.getValue() );
            }
        } );
        
    }
}