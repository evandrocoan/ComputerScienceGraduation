/**
 * Pacote principal que contém o Homebroker.
 */
package homeBroker;

import java.util.ArrayList;

import javax.swing.JOptionPane;

import testes.DriverClass;

/**
 * @author Professional
 *
 */
public class Principal
{
    
    /**
     * @param args
     */
    public static void main( String[] args )
    {
        ArrayList< Conta > contasTeste =
                DriverClass.criarContasFicticia( 1, "123" );
        
        Principal.menuPrincipal(
                loginNoSistema( contasTeste,
                        DriverClass.contasTesteToString( contasTeste ) ),
                contasTeste );
    }
    
    /**
     * Inicia o processo de criação da conta de um usuário do sistema
     * 
     * @return conta a conta criada
     */
    public static Conta criarUsuario()
    {
        String nome = JOptionPane.showInputDialog( "Digite seu nome:" );
        String senha = JOptionPane.showInputDialog( "Digite sua senha:" );
        Conta conta = new Conta( nome, senha, 0, false, new Inventario() );
        // ( String nome, String senha, double saldo,boolean
        // administrador, Inventario inventario )
        return conta;
    }
    
    /**
     * Método de realiza o login no sistema.
     * 
     * @param contas um ArrayList de contas que servirá de bando de dados
     * @param dica uma dica que será aprensetada no menu do login. Inicialmente
     *            ela serve para exibir quais contas estão disponiveis para
     *            login e sua senha
     * @return conta a conta que foi autenticada no sistema, retorna null caso
     *         nenhuma conta tenha sido autenticado
     */
    @SuppressWarnings( "null" )
    public static Conta loginNoSistema( ArrayList< Conta > contas, String dica )
    {
        Conta login = null;
        String command = " ", usuario = " ", senha = " ";
        boolean inputError = false;
        
        if( dica != null )
        {
            if( dica.equals( "" ) )
            {
                dica = "(" + dica + ")";
            }
            
        }
        
        while( !command.equals( "sair" ) && !usuario.equals( "sair" )
                && !senha.equals( "sair" ) )
        {
            usuario =
                    JOptionPane.showInputDialog( ( inputError
                            ? "Usuário ou senha inválidos\n\n" : "" )
                            + "Insira qual conta será feito login: "
                            + ( dica.equals( null )? "" : dica ) );
            if( usuario == null )
            {
                break;
            }
            inputError = false;
            senha =
                    JOptionPane.showInputDialog( "Insira qual senha "
                            + "para a conta: " + usuario );
            
            if( senha == null )
            {
                break;
            }
            
            for( Conta conta: contas )
            {
                if( conta.getNome().equals( usuario ) )
                {
                    command = "sa";
                }
                if( conta.checkSenha( senha ) )
                {
                    command += "ir";
                    
                    if( command.equalsIgnoreCase( "sair" ) )
                    {
                        login = conta;
                        break;
                    }
                }
            }
            inputError = true;
        }
        
        return login;
    }
    
    /**
     * Menu principal que exibe as opções de operação no mercado e na carteira
     * de ações do cliente.
     * 
     * @param conta a conta para qual se estará operando o inventário e no
     *            merdado de ações.
     * @param contasTeste as contasTeste que serão utilizadas para simular a
     *            adição de contas no sistema, isto é, as contas criadas somente
     *            existirão temporariamente.
     */
    public static void menuPrincipal( Conta conta,
            @SuppressWarnings( "unused" ) ArrayList< Conta > contasTeste )
    {
        if( conta == null )
        {
            System.exit( 0 );
        }
        
        String menuOptions =
                new String( "Bem-vindo ao sistema "
                        + "tabajara de cadastro de ações\n"
                        + "Digite 'sair' para fechar o programa.\n"
                        + "Digite 'v' para para ver o inventario\n"
                        // + "Digite 'c' para para criar uma conta!\n"
                        + "Digite 'm' para ver o mercado!\n" );
        String command = "";
        boolean inputError = false;
        
        while( !command.equals( "sair" ) )
        {
            command =
                    JOptionPane.showInputDialog( inputError
                            ? "Você digitou uma opção inválida!\n\n"
                                    + menuOptions : menuOptions );
            if( command == null )
            {
                command = "sair";
            }
            inputError = false;
            
            switch( command )
            {
            case "sair":
                System.exit( 0 );
                break;
            case "v":
                Inventario.exibirInventario( conta );
                break;
            // case "c":
            // Conta novaConta = Principal.criarUsuario();
            // contasTeste.add( novaConta );
            // break;
            case "m":
                Thread threadPrincipal =
                        new Thread( BookDeOfertas.getInstance() );
                threadPrincipal.start();
                break;
            default:
                inputError = true;
                break;
            }
        }
    }
}
