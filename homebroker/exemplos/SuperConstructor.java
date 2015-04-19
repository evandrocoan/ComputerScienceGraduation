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
import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.WindowConstants;

/**
 * @author http://stackoverflow.com/users/714968/mkorbel
 * @see <a
 *      href="http://stackoverflow.com/questions/8976874/show-two-dialogs-on-top-of-each-other-using-java-swing">
 *      show two dialogs on top of each other using java swing</a>
 */
public class SuperConstructor extends JFrame
{
    
    private class FirstDialog extends JDialog
    {
        
        private static final long serialVersionUID = 1L;
        
        FirstDialog( final Frame parent )
        {
            super( parent, "FirstDialog" );
            this.setPreferredSize( new Dimension( 200, 200 ) );
            this.setLocationRelativeTo( parent );
            this.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
            this.setModalityType( Dialog.ModalityType.DOCUMENT_MODAL );
            JButton bNext = new JButton( "Show next dialog" );
            bNext.addActionListener( new ActionListener()
            {
                
                @SuppressWarnings( "unused" )
                @Override
                public void actionPerformed( ActionEvent evt )
                {
                    new SecondDialog( parent, false );
                }
            } );
            this.add( bNext, BorderLayout.NORTH );
            JButton bClose = new JButton( "Close" );
            bClose.addActionListener( new ActionListener()
            {
                
                @SuppressWarnings( "unused" )
                @Override
                public void actionPerformed( ActionEvent evt )
                {
                    FirstDialog.this.setVisible( false );
                }
            } );
            this.add( bClose, BorderLayout.SOUTH );
            this.pack();
            this.setVisible( true );
        }
    }
    
    private class SecondDialog extends JDialog
    {
        
        private static final long serialVersionUID = 1L;
        
        @SuppressWarnings( { "unused" } )
        SecondDialog( final Frame parent, boolean modal )
        {
            // super(parent); // < --- Makes this dialog
            // unfocusable as long as FirstDialog is visible
            this.setPreferredSize( new Dimension( 200, 200 ) );
            this.setLocation( 300, 50 );
            this.setModal( modal );
            this.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
            this.setTitle( "SecondDialog " + ( SuperConstructor.this.i++ ) );
            JButton bClose = new JButton( "Close" );
            bClose.addActionListener( new ActionListener()
            {
                
                @Override
                public void actionPerformed( ActionEvent evt )
                {
                    SecondDialog.this.setVisible( false );
                }
            } );
            this.add( bClose, BorderLayout.SOUTH );
            this.pack();
            this.setVisible( true );
        }
    }
    
    private static final long serialVersionUID = 1L;
    
    /**
     * @param args
     */
    public static void main( String args[] )
    {
        EventQueue.invokeLater( new Runnable()
        {
            
            @SuppressWarnings( "unused" )
            @Override
            public void run()
            {
                new SuperConstructor();
            }
        } );
    }
    
    private int i;
    
    /**
     * 
     */
    public SuperConstructor()
    {
        this.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
        this.setPreferredSize( new Dimension( 300, 300 ) );
        this.setTitle( "Super constructor" );
        Container cp = this.getContentPane();
        JButton b = new JButton( "Show dialog" );
        b.addActionListener( new ActionListener()
        {
            
            @SuppressWarnings( "unused" )
            @Override
            public void actionPerformed( ActionEvent evt )
            {
                new FirstDialog( SuperConstructor.this );
            }
        } );
        cp.add( b, BorderLayout.SOUTH );
        JButton bClose = new JButton( "Close" );
        bClose.addActionListener( new ActionListener()
        {
            
            @SuppressWarnings( "unused" )
            @Override
            public void actionPerformed( ActionEvent evt )
            {
                System.exit( 0 );
            }
        } );
        this.add( bClose, BorderLayout.NORTH );
        this.pack();
        this.setVisible( true );
    }
}
