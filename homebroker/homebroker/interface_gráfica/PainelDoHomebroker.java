package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

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
   
   /**
    * Contém o motor principal.
    */
   private static MotorDoHomebroker motor = MotorDoHomebroker.getInstância();
   
   private static final JanelaDeCadastro JANELA_DE_CADASTRO = JanelaDeCadastro.getInstância();
   private static final JanelaDeVendas JANELA_DE_VENDAS = JanelaDeVendas.getInstância();
   private static final JanelaDeOfertas JANELA_DE_OFERTAS = JanelaDeOfertas.getInstância();
   
   private final JButton botãoDeTeste1 = new JButton( "Adicionar Venda Teste" );
   private final JButton botãoDeTeste2 = new JButton( "Adicionar Compra Teste" );
   
   private final JButton botãoDeOfertas = new JButton( "Janela de Ofertas" );
   private final JButton botãoDeVendas = new JButton( "Janela de Vendas" );
   private final JTextArea campoDeAjudaTexto = new JTextArea( this.campoDeAjuda );
   private final String campoDeAjuda = "Bem-vindo ao sistema tabajara de cadastro de ações.\n"
      + "Digite 's' para fechar o programa.\n" + "Digite 'i' para para ver o inventario.\n"
      + "Digite 'b' para para bloquear uma conta de usuário.\n"
      + "Digite 'ov' para enviar uma ordem de venda.\n"
      + "Digite 'oc' para criar um ordem de compra.\n" + "Digite 'ex' para excluir uma conta.\n"
      + "Digite 'c' para para criar uma conta.\n" + "Digite 'm' para ver o mercado.\n"
      + "Digite 'v' para ver as vendas.\n" + "Digite 't' para adicionar ofertas de teste.";
   
   /**
    * Campo onde para entrada de comandos para o programa em forma de texto.
    */
   private JTextField entradaDeComandos;
   
   /**
    * Botão que envia ao usuário os comandos que estão no campo entradaDeComandos.
    */
   private JButton botãoDeComandos;
   
   /**
    * Cria um painel para colocar os botões, caixas de texto, ...
    */
   private PainelDoHomebroker()
   {
      // Define o gerenciador de layout utilizado.
      super.setLayout( new BorderLayout() );
      
      // Configura os componentes
      this.configurarEntradaDeComandos();
      this.configurarBotãoDeComandos();
      this.configurarBotãoDeOfertas();
      this.configurarBotãoDeVendas();
      this.configurarBotãoDeTeste1();
      this.configurarBotãoDeTeste2();
      this.campoDeAjudaTexto.setEditable( false );
      this.campoDeAjudaTexto.setFocusable( false );
      
      // Configura um painel de botões.
      final JPanel painelDeBotões = new JPanel( new GridBagLayout() );
      final GridBagConstraints gbc = new GridBagConstraints();
      gbc.gridx = 0;
      gbc.gridy = -1;
      gbc.gridwidth = 1;
      painelDeBotões.add( this.botãoDeTeste1, gbc );
      gbc.gridx = 0;
      gbc.gridy = -1;
      gbc.gridwidth = 1;
      painelDeBotões.add( this.botãoDeTeste2, gbc );
      
      final GridBagConstraints separatorConstraint = new GridBagConstraints();
      separatorConstraint.gridx = 0;
      separatorConstraint.gridy = -1;
      separatorConstraint.gridwidth = 1;
      separatorConstraint.weighty = 1.0;
      painelDeBotões.add( new JSeparator( SwingConstants.HORIZONTAL ), separatorConstraint );
      
      gbc.gridx = 0;
      gbc.gridy = -1;
      gbc.gridwidth = 1;
      painelDeBotões.add( this.botãoDeOfertas, gbc );
      gbc.gridx = 0;
      gbc.gridy = -1;
      gbc.gridwidth = 1;
      painelDeBotões.add( this.botãoDeVendas, gbc );
      
      // Adiciona os componentes ao painel principal
      this.add( this.botãoDeComandos, BorderLayout.WEST );
      this.add( this.entradaDeComandos, BorderLayout.NORTH );
      this.add( this.campoDeAjudaTexto, BorderLayout.EAST );
      this.add( painelDeBotões, BorderLayout.CENTER );
      
      Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL, 22 ) );
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
   
   private void adicionarOfertasTeste()
   {
      MotorDoHomebroker.getInstância().adicionarOfertaDeCompra( 10, 3, "Tabajara SA" );
      MotorDoHomebroker.getInstância().adicionarOfertaDeVenda( 10, 10, "Tabajara SA" );
   }
   
   /**
    * Cria o botão principal para enviar os comandos da caixa de texto principal.
    */
   private void configurarBotãoDeComandos()
   {
      this.botãoDeComandos = new JButton( "Enviar comando" );
      this.botãoDeComandos.addActionListener( new ActionListener()
      {
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            PainelDoHomebroker.this.enviarCommando( PainelDoHomebroker.this.conteúdo() );
         }
      } );
      this.botãoDeComandos.setPreferredSize( new Dimension( 200, 35 ) );
      this.botãoDeComandos.setFocusable( false );
   }
   
   private void configurarBotãoDeOfertas()
   {
      final JanelaDeOfertas JANELA_DE_OFERTAS = PainelDoHomebroker.JANELA_DE_OFERTAS;
      
      this.botãoDeOfertas.addActionListener( new ActionListener()
      {
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            JANELA_DE_OFERTAS.setVisible( true );
         }
      } );
      this.botãoDeOfertas.setPreferredSize( new Dimension( 220, 35 ) );
      this.botãoDeOfertas.setFocusable( false );
   }
   
   private void configurarBotãoDeTeste1()
   {
      this.botãoDeTeste1.addActionListener( new ActionListener()
      {
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            MotorDoHomebroker.getInstância().adicionarOfertaDeVenda( 10, 10, "Tabajara SA" );
         }
      } );
      this.botãoDeTeste1.setPreferredSize( new Dimension( 270, 35 ) );
      this.botãoDeTeste1.setFocusable( false );
   }
   
   private void configurarBotãoDeTeste2()
   {
      this.botãoDeTeste2.addActionListener( new ActionListener()
      {
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            MotorDoHomebroker.getInstância().adicionarOfertaDeCompra( 10, 3, "Tabajara SA" );
         }
      } );
      this.botãoDeTeste2.setPreferredSize( new Dimension( 270, 35 ) );
      this.botãoDeTeste2.setFocusable( false );
   }
   
   private void configurarBotãoDeVendas()
   {
      final JanelaDeVendas JANELA_DE_VENDAS = PainelDoHomebroker.JANELA_DE_VENDAS;
      
      this.botãoDeVendas.addActionListener( new ActionListener()
      {
         @Override
         public void actionPerformed( final ActionEvent ae )
         {
            JANELA_DE_VENDAS.setVisible( true );
         }
      } );
      this.botãoDeVendas.setPreferredSize( new Dimension( 220, 35 ) );
      this.botãoDeVendas.setFocusable( false );
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
      
      /**
       * Limpa a caixa de texto ao clicar com o mouse.
       */
      this.entradaDeComandos.addMouseListener( new MouseAdapter()
      {
         @Override
         public void mouseClicked( final MouseEvent e )
         {
            PainelDoHomebroker.this.limpar();
         }
      } );
      
      /**
       * Limpa a caixa de texto ao apertar esc e na primeira vez que se escreve na caixa de texto.
       */
      this.entradaDeComandos.addKeyListener( new KeyAdapter()
      {
         private boolean primeiraVez = true;
         
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
    * Menu principal que exibe as opções de operação no mercado e na carteira de ações do cliente.
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
      case "i":
         this.mostrarInventário();
         break;
      case "b":
         PainelDoHomebroker.JANELA_DE_CADASTRO.efetuarBloqueio();
         break;
      case "c":
         PainelDoHomebroker.JANELA_DE_CADASTRO.efetuarCadastro();
         break;
      case "ov":
         PainelDoHomebroker.JANELA_DE_VENDAS.efetuarVenda();
         break;
      case "ex":
         PainelDoHomebroker.JANELA_DE_CADASTRO.excluirConta();
         break;
      case "oc":
         PainelDoHomebroker.JANELA_DE_VENDAS.efetuarCompra();
         break;
      case "m":
         PainelDoHomebroker.JANELA_DE_OFERTAS.setVisible( true );
         break;
      case "v":
         PainelDoHomebroker.JANELA_DE_VENDAS.setVisible( true );
         break;
      case "t":
         this.adicionarOfertasTeste();
         break;
      default:
         this.imputError();
         break;
      }
   }
   
   private void imputError()
   {
      JOptionPane.showMessageDialog( null, "Você digitou uma " + "opção inválida!\n\n"
         + this.campoDeAjuda );
      this.limpar();
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
         JOptionPane.showMessageDialog( null, "Não há " + "nenhuma conta carregada no sistema!" );
         return;
      }
      JOptionPane.showMessageDialog( null, PainelDoHomebroker.motor.inventarioToString() );
   }
}
