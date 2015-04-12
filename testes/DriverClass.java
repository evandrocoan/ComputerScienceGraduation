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
package testes;

import homebroker.interface_gráfica.Homebroker;
import homebroker.lógica_de_dados.Ação;
import homebroker.lógica_de_dados.BookDeOfertas;
import homebroker.lógica_de_dados.Conta;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;

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
 * Esta classe serve como utiliário que realiza uma serie de test para com a
 * primeira interação do HomeBroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class DriverClass
{
    private static boolean DEBUG = false;
    
    /**
     * Método principal que inicia a execução dos testes
     * 
     * @param args os argumentos passados por linha de comando
     */
    public static void main( String... args )
    {
        Homebroker.main( "teste" );
    }
    
    /**
     * Realiza teste na interface do book de ofertas.
     * 
     * @param contasTeste um ArrayList< Conta > com as contas para testar
     */
    public static void testarBookDeOfertas( ArrayList< Conta > contasTeste )
    {
        BookDeOfertas bookDeOfertas = BookDeOfertas.getInstance();
        for( Conta conta: contasTeste )
        {
            ArrayList< Ação > listaDeAções =
                    conta.getInventario().getListaDeAções();
            
            for( Ação ação: listaDeAções )
            {
                bookDeOfertas.adicionarOfertaDeVenda( ação.getPreço(),
                        ação.getQuantidade(), ação.getNome() );
            }
        }
        if( DriverClass.DEBUG )
        {
            JOptionPane
                    .showMessageDialog( null, "Estou em testarBookDeOfertas" );
        }
    }
    
    /**
     * Método responsável por testar o sistema de login. Ele cria contas de
     * teste, exibe o sistema de login com dica de conta
     */
    public void testarContas()
    {
        // TODO
    }
    
    /**
     * Transforma um ArrayList de contas e uma String
     * 
     * @param contas um ArrayList contendo as contas
     * @return texto um texto contendo os nomes das contas de teste criadas
     */
    public static String contasTesteToString( ArrayList< Conta > contas )
    {
        String texto = "";
        for( Conta conta: contas )
        {
            texto = texto + conta.getNome() + ", ";
        }
        return texto;
    }
    
    /**
     * Um exemplo de implementação de interface.
     */
    public static void testarInterfaceExemplo()
    {
        JFrame frame = new JFrame( "Option Pane Text Area Example" );
        
        final SpringLayout layout = new SpringLayout();
        
        final JPanel panel = new JPanel( layout );
        panel.setPreferredSize( new Dimension( 250, 160 ) );
        
        JLabel lblName = new JLabel( "Name" );
        panel.add( lblName );
        JTextField txtName = new JTextField( 10 );
        txtName.setBorder( BorderFactory.createLineBorder( Color.black ) );
        panel.add( txtName );
        
        JLabel lblAddress = new JLabel( "Address" );
        panel.add( lblAddress );
        JTextArea txtAddress = new JTextArea();
        txtAddress.setBorder( BorderFactory.createLineBorder( Color.black ) );
        txtAddress.setLineWrap( true );
        txtAddress.setWrapStyleWord( true );
        JScrollPane scrollPane =
                new JScrollPane( txtAddress,
                        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS );
        scrollPane.setPreferredSize( new Dimension( 250, 100 ) );
        panel.add( scrollPane );
        
        layout.putConstraint( SpringLayout.WEST, lblName, 0, SpringLayout.WEST,
                panel );
        layout.putConstraint( SpringLayout.NORTH, lblAddress, 10,
                SpringLayout.SOUTH, lblName );
        
        layout.putConstraint( SpringLayout.WEST, txtName, 25,
                SpringLayout.EAST, lblName );
        layout.putConstraint( SpringLayout.NORTH, scrollPane, 10,
                SpringLayout.SOUTH, lblAddress );
        
        int result =
                JOptionPane
                        .showConfirmDialog( frame, panel,
                                "Text Box and Text Area Example",
                                JOptionPane.OK_CANCEL_OPTION,
                                JOptionPane.PLAIN_MESSAGE );
        
        if( result == JOptionPane.YES_OPTION )
        {
            System.out.println( txtName.getText() + ",\n"
                    + txtAddress.getText() );
        } else
        {
            System.out.println( "Canceled" );
        }
        
        System.exit( 0 );
    }
}
