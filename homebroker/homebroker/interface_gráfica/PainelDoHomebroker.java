package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import util.Biblioteca;

/**
 * Representa o painel principal da janela principal.
 *
 * @author Professional
 */
public final class PainelDoHomebroker extends JPanel
{
   /**
    * Contém a única instância do painel.
    */
   private static PainelDoHomebroker instância;
   
   private static MotorDoHomebroker motor = MotorDoHomebroker.getInstância();
   
   /**
    * Contém o motor principal.
    */
   private static final JanelaDeCadastro JANELA_DE_CADASTRO =
            homebroker.interface_gráfica.JanelaDeCadastro.getInstância();
   
   private static final JanelaDeVendas JANELA_DE_VENDAS = JanelaDeVendas
            .getInstância( PainelDoHomebroker.motor );
   
   private static final JanelaDeOfertas JANELA_DE_OFERTAS = JanelaDeOfertas
            .getInstância();
   
   /**
    * Campo onde para entrada de comandos para o programa em forma de texto.
    */
   private transient JTextField entradaDeComandos;
   
   /**
    * Botão que envia ao usuário os comandos que estão no campo
    * entradaDeComandos.
    */
   private transient JButton botãoDeComandos;
   
   /**
    * Contém as informações de apresentação e como utilizar o programa. Tais
    * informações serão apresentadas na interface gráfica ao usuário.
    */
   private final String campoDeAjuda = "Bem-vindo ao sistema "
            + "tabajara de cadastro de ações.\n"
            + "Digite 's' para fechar o programa.\n"
            + "Digite 'v' para para ver o inventario.\n"
            + "Digite 'b' para para bloquear uma conta de usuário.\n"
            + "Digite 'ov' para enviar uma ordem de venda.\n"
            + "Digite 'oc' para criar um ordem de compra.\n"
            + "Digite 'ex' para excluir uma conta.\n"
            + "Digite 'c' para para criar uma conta.\n"
            + "Digite 'm' para ver o mercado.\n";
   
   /**
    * Cria um painel para colocar os botões, caixas de texto, ...
    */
   private PainelDoHomebroker()
   {
      // Liga o book de ofertas
      final Thread processoDoBook =
               new Thread( PainelDoHomebroker.JANELA_DE_OFERTAS );
      processoDoBook.start();
      
      // Configura os componentes
      this.configurarEntradaDeComandos();
      this.configurarBotãoDeComandos();
      
      final JTextArea campoDeAjuda = new JTextArea( this.campoDeAjuda );
      campoDeAjuda.setEditable( false );
      campoDeAjuda.setFocusable( false );
      
      // Define o gerenciador de layout utilizado.
      super.setLayout( new BorderLayout() );
      
      // Adiciona os componentes ao painel principal
      this.add( this.botãoDeComandos, BorderLayout.WEST );
      this.add( this.entradaDeComandos, BorderLayout.NORTH );
      this.add( campoDeAjuda, BorderLayout.EAST );
      
      Biblioteca.trocarFontes( this,
               new Font( this.getName(), Frame.NORMAL, 20 ) );
   }
   
   /**
    * @return instância uma instância da janela de login.
    */
   public static PainelDoHomebroker getInstância()
   {
      synchronized( PainelDoHomebroker.class )
      {
         if( PainelDoHomebroker.instância == null )
         {
            PainelDoHomebroker.instância = new PainelDoHomebroker();
         }
      }
      return PainelDoHomebroker.instância;
   }
   
   private void bloquearUmUsuário()
   {
      PainelDoHomebroker.JANELA_DE_CADASTRO.efetuarBloqueio();
   }
   
   /**
    * Inicia o processo de criação da conta de um usuário do sistema
    *
    * //@return conta a conta criada
    */
   public void cadastrarUsuário()
   {
      PainelDoHomebroker.JANELA_DE_CADASTRO.efetuarCadastro();
   }
   
   /**
    * Cria o botão principal para enviar os comandos da caixa de texto
    * principal.
    */
   private void configurarBotãoDeComandos()
   {
      this.botãoDeComandos = new JButton( "Enviar comando" );
      this.botãoDeComandos.addActionListener( new ActionListener()
      {
         /**
          * Processa o comando na caixa de texto principal
          */
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            PainelDoHomebroker.this.enviarCommando( ae.getActionCommand() );
         }
      } );
      
      // Configura o botão principal
      this.botãoDeComandos.setPreferredSize( new Dimension( 250, 35 ) );
      this.botãoDeComandos.setFocusable( false );
   }
   
   /**
    * Configura o campo de texto para entrada de comandos para o programa.
    */
   private void configurarEntradaDeComandos()
   {
      this.entradaDeComandos = new JTextField( "  Insira qual seu comando  " );
      
      this.entradaDeComandos.addActionListener( new ActionListener()
      {
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            PainelDoHomebroker.this.enviarCommando( ae.getActionCommand() );
         }
      } );
      
      this.entradaDeComandos.addMouseListener( new MouseAdapter()
      {
         /**
          * Limpa a caixa de texto ao clicar com o mouse.
          */
         @Override
         public void mouseClicked( final MouseEvent e )
         {
            PainelDoHomebroker.this.limpar();
         }
      } );
      
      this.entradaDeComandos.addKeyListener( new KeyAdapter()
      {
         private boolean primeiraVez = true;
         
         /**
          * Limpa a caixa de texto ao apertar esc e na primeira vez que se
          * escreve na caixa de texto.
          */
         @Override
         public void keyPressed( final KeyEvent evt )
         {
            if( ( evt.getKeyCode() == KeyEvent.VK_ESCAPE ) || this.primeiraVez )
            {
               PainelDoHomebroker.this.limpar();
            }
            this.primeiraVez = false;
         }
      } );
      
      // Configura a caixaDeTextoPrincipal
      this.entradaDeComandos.setPreferredSize( new Dimension( 250, 35 ) );
   }
   
   /**
    * @return conteúdo o conteúdo da caixa de texto principal.
    */
   String conteúdo()
   {
      return this.entradaDeComandos.getText();
   }
   
   /**
    * Chama a janela responsável por realizar venda da ação.
    */
   private void efetuarCompra()
   {
      PainelDoHomebroker.JANELA_DE_VENDAS.efetuarCompra();
   }
   
   /**
    * Chama a janela responsável por realizar venda da ação.
    */
   private void efetuarVenda()
   {
      PainelDoHomebroker.JANELA_DE_VENDAS.efetuarVenda();
   }
   
   /**
    * Menu principal que exibe as opções de operação no mercado e na carteira de
    * ações do cliente.
    */
   protected void enviarCommando( String comando )
   {
      if( comando == null )
      {
         comando = "s";
      }
      
      switch( comando )
      {
      case "s":
         MotorDoHomebroker.sairDoSistema();
         break;
      case "v":
         this.mostrarInventário();
         break;
      case "b":
         this.bloquearUmUsuário();
         break;
      case "c":
         this.cadastrarUsuário();
         break;
      case "ov":
         this.efetuarVenda();
         break;
      case "ex":
         this.excluirConta();
         break;
      case "oc":
         this.efetuarCompra();
         break;
      case "m":
         this.exibirBookDeOfertas();
         break;
      default:
         this.imputError();
         break;
      }
   }
   
   private void excluirConta()
   {
      PainelDoHomebroker.JANELA_DE_CADASTRO.excluirConta();
   }
   
   /**
    * Exibe o book de ofertas.
    */
   private void exibirBookDeOfertas()
   {
      PainelDoHomebroker.JANELA_DE_OFERTAS.setVisible( true );
   }
   
   private void imputError()
   {
      JOptionPane.showMessageDialog( null, "Você digitou uma "
               + "opção inválida!\n\n" + this.campoDeAjuda );
   }
   
   /**
    * Limpa o conteúdo da caixa de texto principal.
    */
   void limpar()
   {
      this.entradaDeComandos.setText( "" );
   }
   
   /**
    * Exibe o inventário da conta atualmente autenticada.
    */
   private void mostrarInventário()
   {
      if( !PainelDoHomebroker.motor.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há "
                  + "nenhuma conta carregada no sistema!" );
         return;
      }
      JOptionPane.showMessageDialog( null,
               PainelDoHomebroker.motor.inventarioToString() );
   }
}
