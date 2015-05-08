package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 * Classe que realiza o login do usuário no sistema.
 * 
 * @author Professional
 */
public final class JanelaDeLogin extends JFrame
{
    private static JanelaDeLogin instância;
    
    /**
     * @return instância uma intância da janela de login.
     */
    public static JanelaDeLogin getInstância()
    {
        synchronized( JanelaDeLogin.class )
        {
            if( JanelaDeLogin.instância == null )
            {
                final MotorDoHomebroker motor =
                    MotorDoHomebroker.getInstância();
                JanelaDeLogin.instância = new JanelaDeLogin( motor );
            }
        }
        return JanelaDeLogin.instância;
    }
    
    private final MotorDoHomebroker motor;
    
    private JanelaDeLogin( final MotorDoHomebroker motor )
    {
        this.motor = motor;
    }
    
    /**
     * Método de realiza o login no sistema.
     * 
     * @param darDica uma dica que será aprensetada no menu do login.
     *            Inicialmente ela serve para exibir quais contas estão
     *            disponiveis para login.
     */
    @SuppressWarnings( "all" )
    public void loginNoSistema( final String darDica )
    {
        switch( darDica )
        {
        case "login":
            this.loginNoSistemaInterno( "" );
            break;
        
        case "teste":
            JOptionPane.showMessageDialog( null, "Sessão de teste!" );
            this.motor.loginNoSistemaChecagem( "admin", "admin" );
            break;
        
        case "dica":
            JOptionPane.showMessageDialog( null, "Sessão de teste "
                + "COM dica de contas no login!" );
            final StringBuilder dica = new StringBuilder();
            dica.append( '\n' ).append( this.motor.contasTesteToString() );
            
            this.loginNoSistemaInterno( dica.toString() );
            break;
        
        default:
            System.out.println( "Comando de Login inválido! " + darDica );
            break;
        }
    }
    
    /**
     * @param dica
     * @param motor
     */
    private void loginNoSistemaInterno( final String dica )
    {
        String usuário = "";
        String senha = "";
        boolean inputError = true;
        do
        {
            usuário = JOptionPane.showInputDialog(
                ( inputError? "" : "Usuário ou senha inválidos\n\n" )
                    + "Insira qual conta será feito login: " + dica );
            
            if( ( usuário == null ) )
            {
                break;
            }
            senha = JOptionPane.showInputDialog( "Insira qual senha para a "
                + "conta: " + usuário );
            
            if( ( senha == null ) )
            {
                break;
            }
            inputError = this.motor.loginNoSistemaChecagem( usuário, senha );
            
        } while( !inputError );
        
        if( ( usuário == null ) | ( senha == null ) )
        {
            System.exit( 0 );
        }
    }
}
