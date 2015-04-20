/**
 * Pacote que contém a classe principal de testes.
 * 
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
package homebroker.exemplos;

import javax.swing.JFrame;

/**
 * // Fig. 22.12: DesktopTest.java // Demonstrating JDesktopPane.
 * 
 * @author Deitel & Associates
 */
public class DesktopTest {
  /**
   * @param args
   */
  public static void main( final String args[] ) {
    final DesktopFrame desktopFrame = new DesktopFrame();
    desktopFrame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    desktopFrame.setSize( 600, 480 ); // set frame size
    desktopFrame.setVisible( true ); // display frame
  } // end main
} // end class DesktopTest

/**************************************************************************
 * (C) Copyright 1992-2005 by Deitel & Associates, Inc. and * Pearson Education, Inc. All Rights
 * Reserved. * * DISCLAIMER: The authors and publisher of this book have used their * best efforts
 * in preparing the book. These efforts include the * development, research, and testing of the
 * theories and programs * to determine their effectiveness. The authors and publisher make * no
 * warranty of any kind, expressed or implied, with regard to these * programs or to the
 * documentation contained in these books. The authors * and publisher shall not be liable in any
 * event for incidental or * consequential damages in connection with, or arising out of, the *
 * furnishing, performance, or use of these programs. *
 *************************************************************************/
