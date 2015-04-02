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
    
    private static final long serialVersionUID = 1L;
    
    /**
     * 
     */
    public ListPanel()
    {
        setLayout( new GridLayout( 0, 2, 10, 10 ) );
        DefaultListModel< JButton > model = new DefaultListModel<>();
        model.addElement( createButtons( "one" ) );
        model.addElement( createButtons( "two" ) );
        model.addElement( createButtons( "three" ) );
        model.addElement( createButtons( "four" ) );
        model.addElement( createButtons( "five" ) );
        model.addElement( createButtons( "six" ) );
        model.addElement( createButtons( "seven" ) );
        model.addElement( createButtons( "eight" ) );
        model.addElement( createButtons( "nine" ) );
        model.addElement( createButtons( "ten" ) );
        model.addElement( createButtons( "eleven" ) );
        model.addElement( createButtons( "twelwe" ) );
        JList< JButton > list = new JList<>( model );
        list.setCellRenderer( new PanelRenderer() );
        JScrollPane scroll1 = new JScrollPane( list );
        final JScrollBar scrollBar = scroll1.getVerticalScrollBar();
        scrollBar.addAdjustmentListener( new AdjustmentListener()
        {
            
            @SuppressWarnings( "unused" )
            @Override
            public void adjustmentValueChanged( AdjustmentEvent e )
            {
                System.out.println( "JScrollBar's current value = "
                        + scrollBar.getValue() );
            }
        } );
        add( scroll1 );
        JScrollPane scroll2 = new JScrollPane( createPanel() );
        add( scroll2 );
        final JScrollBar scrollBar1 = scroll2.getVerticalScrollBar();
        scrollBar1.addAdjustmentListener( new AdjustmentListener()
        {
            
            @SuppressWarnings( "unused" )
            @Override
            public void adjustmentValueChanged( AdjustmentEvent e )
            {
                System.out.println( "JScrollBar's current value = "
                        + scrollBar1.getValue() );
            }
        } );
        
    }
    
    /**
     * @return
     */
    @SuppressWarnings( "javadoc" )
    public static JPanel createPanel()
    {
        JPanel panel = new JPanel();
        panel.setLayout( new GridLayout( 0, 1, 1, 1 ) );
        panel.add( createButtons( "one" ) );
        panel.add( createButtons( "two" ) );
        panel.add( createButtons( "three" ) );
        panel.add( createButtons( "four" ) );
        panel.add( createButtons( "five" ) );
        panel.add( createButtons( "six" ) );
        panel.add( createButtons( "seven" ) );
        panel.add( createButtons( "eight" ) );
        panel.add( createButtons( "nine" ) );
        panel.add( createButtons( "ten" ) );
        panel.add( createButtons( "eleven" ) );
        panel.add( createButtons( "twelwe" ) );
        return panel;
    }
    
    /**
     * @param text
     * @return
     */
    @SuppressWarnings( "javadoc" )
    public static JButton createButtons( String text )
    {
        JButton button = new JButton( text );
        return button;
    }
    
    /**
     * @param args
     */
    public static void main( String[] args )
    {
        SwingUtilities.invokeLater( new Runnable()
        {
            
            @Override
            public void run()
            {
                ListPanel frame = new ListPanel();
                frame.setDefaultCloseOperation( EXIT_ON_CLOSE );
                // frame.pack();
                frame.setSize( 270, 200 );
                frame.setVisible( true );
            }
        } );
    }
    
    class PanelRenderer implements ListCellRenderer< Object >
    {
        
        @SuppressWarnings( "unused" )
        @Override
        public Component getListCellRendererComponent( JList< ? > list,
                Object value, int index, boolean isSelected,
                boolean cellHasFocus )
        {
            JButton renderer = (JButton) value;
            renderer.setBackground( isSelected? Color.red : list
                    .getBackground() );
            return renderer;
        }
    }
}