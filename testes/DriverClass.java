/**
 * Pacote que contém a classe principal de testes.
 */
package testes;

import homeBroker.Ação;
import homeBroker.Biblioteca;
import homeBroker.BookDeOfertas;
import homeBroker.Conta;
import homeBroker.Inventario;
import homeBroker.JanelaDoBook;
import homeBroker.JanelaPrincipal;
import homeBroker.ProgramaPrincipal;

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
    /**
     * 
     */
    public static ProgramaPrincipal programaPrincipal;
    
    /**
     * 
     */
    public static JanelaDoBook janelaDoBook;
    
    /**
     * Método principal que inicia a execução dos testes
     * 
     * @param args os argumentos passados por linha de comando
     */
    public static void main( String... args )
    {
        // Inicializa as interfaces gráficas para se realizar os testes.
        DriverClass.programaPrincipal = ProgramaPrincipal.getInstance();
        DriverClass.janelaDoBook = JanelaDoBook.getInstance();
        
        DriverClass.testarBookDeOfertas();
    }
    
    /**
     * Realiza teste na interface do book de ofertas.
     */
    public static void testarBookDeOfertas()
    {
        for( Conta conta: programaPrincipal.contasTeste )
        {
            ArrayList< Ação > listaDeAções =
                    conta.getInventario().getListaDeAções();
            
            for( Ação ação: listaDeAções )
            {
                BookDeOfertas.getInstance().adicionarOfertaDeVenda( ação );
            }
        }
        JOptionPane.showMessageDialog( null, "Estou em testarBookDeOfertas" );
        ProgramaPrincipal.janelaPrincipal =
                new JanelaPrincipal( "HomeBroker Tabajara", programaPrincipal,
                        janelaDoBook );
    }
    
    /**
     * Método responsável por testar o sistema de login. Ele cria contas de
     * teste, exibe o sistema de login com dica de conta
     */
    public void testarContas()
    {
        // ######################## Cria um conta para apresentação do sistema
        // ao usuario ########################################################
        // ArrayList< Conta > contasTeste =
        // DriverClass.criarContasFicticia( 11, "123" );
        
        // faz login no sistema
        DriverClass.programaPrincipal.loginNoSistema( DriverClass
                .contasTesteToString( programaPrincipal.contasTeste )
                + "Senha geral: 123" );
        
        programaPrincipal.menuPrincipal( null );
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
     * Cria contas teste para o sistema.
     * 
     * @param quantidade a quantidade de contas teste para se criar
     * @param senha senha que as contas de teste terão
     * @return conta uma nova conta teste com dados fictícios
     */
    public static ArrayList< Conta > criarContasFicticia( int quantidade,
            String senha )
    {
        ArrayList< Conta > contasTeste = new ArrayList<>();
        
        for( int i = 0; i < quantidade; i++ )
        {
            Conta contaTeste =
                    new Conta( "User" + Biblioteca.gerarNumeroAleatorio(),
                            senha,
                            2000.5 * homeBroker.Biblioteca
                                    .gerarNumeroAleatorio(), false,
                            new Inventario() );
            DriverClass.criarInventarioFicticio( contaTeste, quantidade );
            
            contasTeste.add( contaTeste );
        }
        JOptionPane.showMessageDialog( null, "Estou em criarContasFictícias "
                + contasTeste.get( 0 ).getNome() );
        return contasTeste;
    }
    
    /**
     * Cria um inventário fictício de ações contendo 5 ações fictícias
     * 
     * @param conta a conta que irá receber as ações fictícioas
     * @param quantidade a quantidade de ações fictícias para se criar
     */
    public static void criarInventarioFicticio( Conta conta, int quantidade )
    {
        for( int i = 0; i < quantidade / 5; i++ )
        {
            conta.getInventario().adicionarAoInventario(
                    new Ação( 2.2 + homeBroker.Biblioteca
                            .gerarNumeroAleatorio(), 10 + homeBroker.Biblioteca
                            .gerarNumeroAleatorio(), "Tabajara SA"
                            + homeBroker.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação( 22.2 + homeBroker.Biblioteca
                            .gerarNumeroAleatorio(),
                            100 + homeBroker.Biblioteca.gerarNumeroAleatorio(),
                            "Tabajara SO"
                                    + homeBroker.Biblioteca
                                            .gerarNumeroAleatorio() ) );
            
            conta.getInventario()
                    .adicionarAoInventario(
                            new Ação( 200.2 + homeBroker.Biblioteca
                                    .gerarNumeroAleatorio(),
                                    1000 + homeBroker.Biblioteca
                                            .gerarNumeroAleatorio(),
                                    "Tabajara SP"
                                            + homeBroker.Biblioteca
                                                    .gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação( 2000.2 + homeBroker.Biblioteca
                            .gerarNumeroAleatorio(),
                            10000 + homeBroker.Biblioteca
                                    .gerarNumeroAleatorio(), "Tabajara ST"
                                    + homeBroker.Biblioteca
                                            .gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação( 200006.2 + homeBroker.Biblioteca
                            .gerarNumeroAleatorio(),
                            10000 + homeBroker.Biblioteca
                                    .gerarNumeroAleatorio(), "Tabajara SS"
                                    + homeBroker.Biblioteca
                                            .gerarNumeroAleatorio() ) );
        }
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
