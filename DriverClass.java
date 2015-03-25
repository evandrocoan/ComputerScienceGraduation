import javax.swing.JOptionPane;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Esta classe serve como utiliário que realiza uma serie de test para com a
 * primeira interação do HomeBroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class DriverClass
{
    ArrayList< String > listOfNames;
    
    /**
     * Método principal que inicia a execução dos testes
     * 
     * @param args os argumentos passados por linha de comando
     */
    public DriverClass()
    {
        listOfNames =
                new ArrayList< String >( Arrays.asList( "PetroBol", "OGY",
                        "PineApple", "Doors", "Sea", "Mike", "Sadida",
                        "BadYear", "Kebab Hut", "Mundo" ) );
    }
    
    public static void main( int args )
    {
        
        GUI g = new GUI();
        // g.add(new Canvas());
        JPanel panel = new JPanel();
        panel.setBackground( Color.WHITE );
        panel.setBounds( 300, 300, 300, 300 );
        Thread t = new Thread( new motor( panel ) );
        t.start();
        g.add( panel, BorderLayout.CENTER );
        
        g.setVisible( true );
    }
    
    public static void main( String[] args )
    {
        DriverClass d = new DriverClass();
        {
            // JFrame frame = new JFrame( "Option Pane Text Area Example" );
            //
            // final SpringLayout layout = new SpringLayout();
            //
            // final JPanel panel = new JPanel( layout );
            // panel.setPreferredSize( new Dimension( 250, 160 ) );
            //
            // JLabel lblName = new JLabel( "Name" );
            // panel.add( lblName );
            // JTextField txtName = new JTextField( 10 );
            // txtName.setBorder( BorderFactory.createLineBorder( Color.black )
            // );
            // panel.add( txtName );
            //
            // JLabel lblAddress = new JLabel( "Address" );
            // panel.add( lblAddress );
            // JTextArea txtAddress = new JTextArea();
            // txtAddress
            // .setBorder( BorderFactory.createLineBorder( Color.black ) );
            // txtAddress.setLineWrap( true );
            // txtAddress.setWrapStyleWord( true );
            // JScrollPane scrollPane =
            // new JScrollPane( txtAddress,
            // JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            // JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS );
            // scrollPane.setPreferredSize( new Dimension( 250, 100 ) );
            // panel.add( scrollPane );
            //
            // layout.putConstraint( SpringLayout.WEST, lblName, 0,
            // SpringLayout.WEST, panel );
            // layout.putConstraint( SpringLayout.NORTH, lblAddress, 10,
            // SpringLayout.SOUTH, lblName );
            //
            // layout.putConstraint( SpringLayout.WEST, txtName, 25,
            // SpringLayout.EAST, lblName );
            // layout.putConstraint( SpringLayout.NORTH, scrollPane, 10,
            // SpringLayout.SOUTH, lblAddress );
            //
            // int result =
            // JOptionPane.showConfirmDialog( frame, panel,
            // "Text Box and Text Area Example",
            // JOptionPane.OK_CANCEL_OPTION,
            // JOptionPane.PLAIN_MESSAGE );
            //
            // if( result == JOptionPane.YES_OPTION )
            // {
            // System.out.println( txtName.getText() + ",\n"
            // + txtAddress.getText() );
            // } else
            // {
            // System.out.println( "Canceled" );
            // System.exit( 0 );
            // }
            
        }
        
        String command =
                new String( "Bem-vindo ao sistema "
                        + "tabajara de cadastro de ações\n "
                        + "Digite 'sair' para fechar o programa.\n"
                        + "Digite 'v' para para ver o inventario\n"
                        // + "Digite 'c' para para criar uma conta!\n"
                        + "Digite 'm' para ver o mercado!\n" );
        JOptionPane.showMessageDialog( null, command );
        
        // ######################## Cria um conta para apresentação do sistema
        // ao usuario ########################################################
        ArrayList< Conta > contasTeste = new ArrayList< Conta >();
        
        for( int i = 1; i < 11; i++ )
        {
            contasTeste.add( DriverClass.criarContaFicticia( d.listOfNames ) );
        }
        
        // faz login no sistema
        Conta conta = contasTeste.get( 0 );
        
        while( !command.equals( "sair" ) )
        {
            
            command = JOptionPane.showInputDialog( "Selecione o comando" );
            
            switch( command )
            {
            case "sair":
                System.exit( 0 );
                return;
            case "v":
                DriverClass.exibirInventario( conta );
                break;
            // case "c":
            // Conta novaConta = DriverClass.criarUsuario();
            // contasTeste.add( novaConta );
            // break;
            case "m":
                main( 1 );
                return;
            }
        }
    }
    
    /**
     * @param conta
     * @return a conta que foi autenticada no sistema.
     */
    private static Conta loginNoSistema( ArrayList< Conta > contas )
    {
        Conta login = null;
        String command = " ", usuario = " ", senha = " ";
        
        while( !command.equals( "sair" ) && !usuario.equals( "sair" )
                && !senha.equals( "sair" ) )
        {
            usuario =
                    JOptionPane.showInputDialog( "Insira qual conta "
                            + "será feito login:" );
            senha =
                    JOptionPane.showInputDialog( "Insira qual senha "
                            + "para a conta " + usuario );
            for( Conta conta: contas )
            {
                if( conta.getNome().equals( usuario ) )
                {
                    command = "sa";
                }
                if( conta.checkSenha( senha ) )
                {
                    command += "ir";
                }
            }
        }
        return login;
    }
    
    /**
     * Inicia o processo de criação da conta de um usuário do sistema.
     */
    private static Conta criarUsuario()
    {
        String nome = JOptionPane.showInputDialog( "Digite seu nome:" );
        String senha = JOptionPane.showInputDialog( "Digite sua senha:" );
        Conta c = new Conta( nome, senha, 0, false, new Inventario() );
        // ( String nome, String senha, double saldo,boolean
        // administrador, Inventario inventario )
        return c;
    }
    
    /**
     * Exibe na tela o inventário do usuário.
     */
    private static void exibirInventario( Conta conta )
    {
        String teste = conta.getInventario().inventarioToString();
        JOptionPane.showMessageDialog( null, teste );
    }
    
    /**
     * Cria contas teste para o sistema.
     * 
     * @return conta uma nova conta teste com dados fictícios.
     */
    private static Conta criarContaFicticia( ArrayList< String > listOfNames )
    {
        Conta contaTeste =
                new Conta( "Usuario" + DriverClass.gerarNumeroAleatorio(),
                        "123", 2000.5 * DriverClass.gerarNumeroAleatorio(),
                        false, new Inventario() );
        
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( listOfNames ) );
        
        // contaTeste.getInventario().adicionarAoInventario( new Acao(
        // listOfNames ) );
        //
        // contaTeste.getInventario().adicionarAoInventario( new Acao(
        // listOfNames ) );
        //
        // contaTeste.getInventario().adicionarAoInventario( new Acao(
        // listOfNames ) );
        //
        // contaTeste.getInventario().adicionarAoInventario( new Acao(
        // listOfNames ) );
        
        return contaTeste;
    }
    
    /**
     * Gera número aleátorios entre 1 e 100;
     * 
     * @return numero um número aleátorio entre 1 e 100
     */
    private static int gerarNumeroAleatorio()
    {
        double random = Math.random();
        
        double x = random * 100 + 1;
        
        int y = (int) x;
        
        return y;
    }
}
