/**
 * Use the plural for packages with homogeneous contents and the singular for packages with
 * heterogeneous contents.
 * 
 * A class is similar to a database relation. A database relation should be named in the singular as
 * its records are considered to be instances of the relation. The function of a relation is to
 * compose a complex record from simple data.
 * 
 * A package, on the other hand, is not a data abstraction. It assists with organization of code and
 * resolution of naming conflicts. If a package is named in the singular, it doesn't mean that each
 * member of the package is an instance of the package; it contains related but heterogeneous
 * concepts. If it is named in the plural (as they often are), I would expect that the package
 * contains homogeneous concepts.
 * 
 * For example, a type should be named "TaskCollection" instead of "TasksCollection," as it is a
 * collection containing instances of a Task. A package named com.myproject.task does not mean that
 * each contained class is an instance of a task. There might be a TaskHandler, a TaskFactory, etc.
 * A package named com.myproject.tasks, however, would contain different types that are all tasks:
 * TakeOutGarbageTask, DoTheDishesTask, etc.
 * 
 * @author http://programmers.stackexchange.com/users/20202/matthew-rodatus
 * @see <a
 *      href="http://programmers.stackexchange.com/questions/75919/should-package-names-be-singular-or-plural">
 *      Should package names be singular or plural?</a>
 */

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpringLayout;

/**
 * 
 * @author Professional
 */
@SuppressWarnings( "javadoc" )
public class SimplesExemploGUI
{
   
   /**
    * @param args
    */
   public static void main( final String[] args )
   {
      SimplesExemploGUI.testarInterfaceExemplo();
   }
   
   /**
    * Um exemplo de implementação de interface.
    */
   public static void testarInterfaceExemplo()
   {
      final JFrame frame = new JFrame( "Option Pane Text Area Example" );
      
      final SpringLayout layout = new SpringLayout();
      
      final JPanel panel = new JPanel( layout );
      panel.setPreferredSize( new Dimension( 250, 160 ) );
      
      final JLabel lblName = new JLabel( "Name" );
      panel.add( lblName );
      final JTextField txtName = new JTextField( 10 );
      txtName.setBorder( BorderFactory.createLineBorder( Color.black ) );
      panel.add( txtName );
      
      final JLabel lblAddress = new JLabel( "Address" );
      panel.add( lblAddress );
      final JTextArea txtAddress = new JTextArea();
      txtAddress.setBorder( BorderFactory.createLineBorder( Color.black ) );
      txtAddress.setLineWrap( true );
      txtAddress.setWrapStyleWord( true );
      final JScrollPane scrollPane = new JScrollPane( txtAddress,
         ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
         ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS );
      scrollPane.setPreferredSize( new Dimension( 250, 100 ) );
      panel.add( scrollPane );
      
      layout.putConstraint( SpringLayout.WEST, lblName, 0, SpringLayout.WEST, panel );
      layout.putConstraint( SpringLayout.NORTH, lblAddress, 10, SpringLayout.SOUTH, lblName );
      
      layout.putConstraint( SpringLayout.WEST, txtName, 25, SpringLayout.EAST, lblName );
      layout.putConstraint( SpringLayout.NORTH, scrollPane, 10, SpringLayout.SOUTH, lblAddress );
      
      final int result = JOptionPane.showConfirmDialog( frame, panel,
         "Text Box and Text Area Example", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE );
      
      if( result == JOptionPane.YES_OPTION )
      {
         System.out.println( txtName.getText() + ",\n" + txtAddress.getText() );
      } else
      {
         System.out.println( "Canceled" );
      }
      
      System.exit( 0 );
   }
}
