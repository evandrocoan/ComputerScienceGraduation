/**
 * 
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import javax.swing.SwingUtilities;

/**
 * 
 * @author Professional
 */
public final class Homebroker
{
    /**
     * Motor responsável pela lógica da interface gráfica principal do programa.
     */
    private static MotorDoHomebroker motor = MotorDoHomebroker
        .getInstância();
    
    private Homebroker()
    {
    }
    
    /**
     * Realiza a leitura dos parâmetros passados por linha de comando. Caso não
     * haja nenhum parâmetro, inicia a interface gráfica de login no sistema.
     * 
     * @param args "Os comandos disponívels:\n" +
     *            "teste: abre o programa em mode de teste sem dica" +
     *            "das contas para se logar\n" +
     *            "dica: o mesmo que teste, mas abre o programa " +
     *            "com dicas de contas para se logar."
     */
    @SuppressWarnings( "all" )
    private static void iniciarSistema( final String[] args )
    {
        if( ( args == null ) || ( args.length == 0 ) )
        {
            Homebroker.modoDeLogin();
        } else
        {
            boolean exitLoop = false;
            
            for( int i = 0; i < args.length; i++ )
            {
                switch( args[i] )
                {
                case "teste":
                    Homebroker.modoDeTeste();
                    break;
                
                case "dica":
                    Homebroker.modoDeDica();
                    break;
                
                case "ajuda":
                    System.out.println( "Comandos disponívels:\n"
                        + "teste: abre o programa em mode de teste sem dica"
                        + "das contas para se logar\n"
                        + "dica: o mesmo que teste, mas abre o programa "
                        + "com dicas de contas para se logar." );
                    exitLoop = true;
                    break;
                
                default:
                    System.out.println( "Linha de Comando inválido! " + args[i] );
                    exitLoop = true;
                    break;
                }
                if( exitLoop )
                {
                    break;
                }
            }
        }
    }
    
    /**
     * Método principal que inicia a execução do programa.
     * 
     * @param args caso receba o argumento 'teste' abre o programa em uma conta
     *            teste.
     */
    public static void main( final String... args )
    {
        Homebroker.iniciarSistema( args );
    }
    
    /**
     * Inicia o sistema em modo de teste exibindo dica de contas para logar.
     */
    private static void modoDeDica()
    {
        // encapsula o motor para evitar o synthetic-access
        final MotorDoHomebroker motor = Homebroker.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeLogin janelaDeLogin;
                final JanelaDoHomebroker janelaDoHomebroker;
                
                janelaDeLogin = JanelaDeLogin.getInstância( motor );
                janelaDoHomebroker = JanelaDoHomebroker.getInstância( motor );
                
                janelaDeLogin.loginNoSistema( "dica" );
                janelaDoHomebroker.setVisible( true );
            }
        } );
    }
    
    /**
     * Inicia o sistema o login no sistema.
     */
    private static void modoDeLogin()
    {
        // encapsula a janela para evitar o synthetic-access
        final MotorDoHomebroker motor = Homebroker.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeLogin janelaDeLogin;
                final JanelaDoHomebroker janelaDoHomebroker;
                
                janelaDeLogin = JanelaDeLogin.getInstância( motor );
                janelaDoHomebroker = JanelaDoHomebroker.getInstância( motor );
                
                janelaDeLogin.loginNoSistema( "login" );
                janelaDoHomebroker.setVisible( true );
            }
        } );
    }
    
    /**
     * Inicia o sistema em modo de teste.
     */
    private static void modoDeTeste()
    {
        // encapsula o motor para evitar o synthetic-access
        final MotorDoHomebroker motor = Homebroker.motor;
        /**
         * Programando um trabalho para o Event Dispatcher Thread. Porque Java
         * Swing não é thread-safe.
         */
        SwingUtilities.invokeLater( new Runnable()
        {
            /**
             * Executa o homebroker.
             */
            @Override
            public void run()
            {
                final JanelaDeLogin janelaDeLogin;
                final JanelaDoHomebroker janelaDoHomebroker;
                
                janelaDeLogin = JanelaDeLogin.getInstância( motor );
                janelaDoHomebroker = JanelaDoHomebroker.getInstância( motor );
                
                janelaDeLogin.loginNoSistema( "teste" );
                janelaDoHomebroker.setVisible( true );
            }
        } );
    }
}
