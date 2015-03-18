import javax.swing.JOptionPane;

/**
 * Esta classe serve como utiliário que realiza uma serie de test para com a
 * primeira interação do HomeBroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class DriverClass
{
    
    /**
     * Método principal que inicia a execução dos testes
     * 
     * @param args os argumentos passados por linha de comando
     */
    public static void main( String[] args )
    {
        String command =
                new String( "Bem-vindo ao sistema "
                        + "tabajara de cadastro de ações\n "
                        + "Digite 'sair' para fechar o programa.\n"
                        + "Digite 'v' para para ver o inventario\n"
                        + "Digite 'c' para para criar uma conta!\n" );
        JOptionPane.showMessageDialog( null, command );
        
        // ######################## Cria um conta para apresentação do sistema
        // ao usuario ########################################################
        Conta contaTeste =
                new Conta( "Evandro", "123", 2000.5, true, new Inventario() );
        
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( 2.2, 10, "Tabajara SA" ) );
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( 22.2, 100, "Tabajara SO" ) );
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( 200.2, 1000, "Tabajara SP" ) );
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( 2000.2, 10000, "Tabajara ST" ) );
        contaTeste.getInventario().adicionarAoInventario(
                new Acao( 200006.2, 10000, "Tabajara SS" ) );
        
        while( !command.equals( "sair" ) )
        {
            command = JOptionPane.showInputDialog( "Selecione o comando" );
            
            switch( command )
            {
            case "sair":
                return;
            case "v":
                String teste = contaTeste.getInventario().inventarioToString();
                JOptionPane.showMessageDialog( null, teste );
                break;
            case "c":
                String nome = JOptionPane.showInputDialog( "Digite seu nome:" );
                String senha =
                        JOptionPane.showInputDialog( "Digite sua senha:" );
                Conta c = new Conta( nome, senha, 0, false, new Inventario() );
                // ( String nome, String senha, double saldo,boolean
                // administrador, Inventario inventario )
            }
        }
    }
}
