/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 *
 * @author Professional
 */
public final class JanelaDoHomebroker extends JFrame
{
   /**
    * Contém a única instância desta classe.
    */
   private static JanelaDoHomebroker instância;
   
   /**
    * Armazenam o painel do homebroker.
    */
   private final PainelDoHomebroker painelPrincipal;
   
   private final MotorDoHomebroker motor;
   
   /**
    * Construtor que cria a janela principal do programa.
    */
   private JanelaDoHomebroker()
   {
      super( "HomeBroker Tabajara" );
      this.motor = MotorDoHomebroker.getInstância();
      
      // Adiciona o painel principal nesta janela
      this.painelPrincipal = PainelDoHomebroker.getInstância();
      this.painelPrincipal.setDoubleBuffered( true );
      this.add( this.painelPrincipal );
      
      // Define que a janela deve fechar ao sair.
      this.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
      
      // Abre a janela maximizado
      this.setLocation( 210, 10 );
      // this.setLocation( 250, 150 );
      // this.setExtendedState( Frame.MAXIMIZED_BOTH );
      
      // Ajusta a janela ao tamanho dos elementos.
      this.pack();
   }
   
   /**
    * @return instância a instância da janela.
    */
   public static JanelaDoHomebroker getInstância()
   {
      synchronized( JanelaDoHomebroker.class )
      {
         if( JanelaDoHomebroker.instância == null )
         {
            JanelaDoHomebroker.instância = new JanelaDoHomebroker();
         }
      }
      return JanelaDoHomebroker.instância;
   }
   
   /**
    * Método de realiza o login no sistema.
    *
    * @param darDica uma dica que será aprensetada no menu do login. Inicialmente ela serve para
    *           exibir quais contas estão disponíveis para login.
    */
   @SuppressWarnings( "all" )
   public void loginNoSistema( final String darDica )
   {
      switch( darDica )
      {
      case "login":
         this.loginNoSistemaInterno( "" );
         this.setVisible( true );
         break;
      
      case "teste":
         this.motor.loginNoSistemaChecagem( "admin", "admin" );
         this.setVisible( true );
         break;
      
      case "dica":
         JOptionPane.showMessageDialog( null, "Sessão de teste " + "COM dica de contas no login!" );
         final StringBuilder dica = new StringBuilder();
         dica.append( '\n' ).append( this.motor.contasTesteToString() );
         
         this.loginNoSistemaInterno( dica.toString() );
         this.setVisible( true );
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
         usuário = JOptionPane
            .showInputDialog( ( inputError? "" : "Usuário ou senha inválidos\n\n" )
               + "Insira qual conta será feito login: " + dica );
         
         if( ( usuário == null ) )
         {
            break;
         }
         senha = JOptionPane.showInputDialog( "Insira qual senha para a " + "conta: " + usuário );
         
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
