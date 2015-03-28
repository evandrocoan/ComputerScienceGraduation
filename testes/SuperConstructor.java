/**
 * Pacote que contém a classe principal de testes.
 */
package testes;

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
 *
 */
public class SuperConstructor extends JFrame
{
    
    private static final long serialVersionUID = 1L;
    
    /**
     * 
     */
    public SuperConstructor()
    {
        setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
        setPreferredSize( new Dimension( 300, 300 ) );
        setTitle( "Super constructor" );
        Container cp = getContentPane();
        JButton b = new JButton( "Show dialog" );
        b.addActionListener( new ActionListener()
        {
            
            @SuppressWarnings( "unused" )
            @Override
            public void actionPerformed( ActionEvent evt )
            {
                FirstDialog firstDialog =
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
        add( bClose, BorderLayout.NORTH );
        pack();
        setVisible( true );
    }
    
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
                SuperConstructor superConstructor = new SuperConstructor();
            }
        } );
    }
    
    private class FirstDialog extends JDialog
    {
        
        private static final long serialVersionUID = 1L;
        
        FirstDialog( final Frame parent )
        {
            super( parent, "FirstDialog" );
            setPreferredSize( new Dimension( 200, 200 ) );
            setLocationRelativeTo( parent );
            setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
            setModalityType( Dialog.ModalityType.DOCUMENT_MODAL );
            JButton bNext = new JButton( "Show next dialog" );
            bNext.addActionListener( new ActionListener()
            {
                
                @SuppressWarnings( "unused" )
                @Override
                public void actionPerformed( ActionEvent evt )
                {
                    SecondDialog secondDialog =
                            new SecondDialog( parent, false );
                }
            } );
            add( bNext, BorderLayout.NORTH );
            JButton bClose = new JButton( "Close" );
            bClose.addActionListener( new ActionListener()
            {
                
                @SuppressWarnings( "unused" )
                @Override
                public void actionPerformed( ActionEvent evt )
                {
                    setVisible( false );
                }
            } );
            add( bClose, BorderLayout.SOUTH );
            pack();
            setVisible( true );
        }
    }
    
    private int i;
    
    private class SecondDialog extends JDialog
    {
        
        private static final long serialVersionUID = 1L;
        
        @SuppressWarnings( { "unused", "static-access" } )
        SecondDialog( final Frame parent, boolean modal )
        {
            // super(parent); // < --- Makes this dialog
            // unfocusable as long as FirstDialog is visible
            setPreferredSize( new Dimension( 200, 200 ) );
            setLocation( 300, 50 );
            setModal( modal );
            setDefaultCloseOperation( JDialog.DISPOSE_ON_CLOSE );
            setTitle( "SecondDialog " + ( SuperConstructor.this.i++ ) );
            JButton bClose = new JButton( "Close" );
            bClose.addActionListener( new ActionListener()
            {
                
                @Override
                public void actionPerformed( ActionEvent evt )
                {
                    setVisible( false );
                }
            } );
            add( bClose, BorderLayout.SOUTH );
            pack();
            setVisible( true );
        }
    }
}
