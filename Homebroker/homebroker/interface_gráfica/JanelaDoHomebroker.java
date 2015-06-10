/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

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
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import util.Biblioteca;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 *
 * @author Professional
 */
public final class JanelaDoHomebroker
{
   private static final JanelaDeCadastro JANELA_DE_CADASTRO;
   private static final JanelaDeVendas JANELA_DE_VENDAS;
   private static final JanelaDeOfertas JANELA_DE_OFERTAS;
   
   private static JanelaDoHomebroker instância;
   
   static
   {
      JANELA_DE_CADASTRO = JanelaDeCadastro.getInstância();
      JANELA_DE_VENDAS = JanelaDeVendas.getInstância();
      JANELA_DE_OFERTAS = JanelaDeOfertas.getInstância();
   }
   
   /**
    * Contém a única instância desta classe.
    */
   
   private final Fachada fachada = Fachada.getInstância();
   
   private JFrame janela;
   private JButton botãoDeTeste1;
   private JButton botãoDeTeste2;
   private JButton botãoDeOfertas;
   private JButton botãoDeVendas;
   
   /**
    * Contém vários botões agrupados como uma lista.
    */
   private JPanel painelDeBotões;
   
   /**
    * Contém as informações dos comandos disponíveis a este programa.
    */
   private JTextArea campoDeAjudaTexto;
   private String campoDeAjuda;
   
   /**
    * Campo onde para entrada de comandos para o programa em forma de texto.
    */
   private JTextField entradaDeComandos;
   
   /**
    * Botão que envia ao usuário os comandos que estão no campo entradaDeComandos.
    */
   private JButton botãoDeComandos;
   
   /**
    * Armazenam o painel do homebroker.
    */
   private JPanel painel;
   
   /**
    * Construtor que cria a janela principal do programa.
    */
   private JanelaDoHomebroker()
   {
      // Liga o book de ofertas
      final Thread processoDeAtualizar = new Thread( new Atualizador() );
      processoDeAtualizar.start();
      
      this.painel = new JPanel( new BorderLayout() );
      this.configurarPainel();
      
      this.janela = new JFrame( "Simulador de HomeBroker" );
      this.configurarJanela();
   }
   
   /**
    * @param nova um boolean true caso precisa destruir a janela anterior e construir uma nova
    *           janela, false caso queira se pegar a janela já existente.
    * @return INSTÂNCIA a instância da janela.
    */
   public static JanelaDoHomebroker getInstância( final boolean nova )
   {
      synchronized( JanelaDoHomebroker.class )
      {
         if( JanelaDoHomebroker.instância == null )
         {
            JanelaDoHomebroker.instância = new JanelaDoHomebroker();
         }
         if( nova )
         {
            JanelaDoHomebroker.instância.finalize();
            JanelaDoHomebroker.instância = new JanelaDoHomebroker();
         }
      }
      return JanelaDoHomebroker.instância;
   }
   
   private void adicionarOfertasTeste()
   {
      if( Utilidades.isAdministradora() )
      {
         Fachada.getInstância().adicionarOfertaDeCompra( 12.63, 3, "Banese-ON53" );
         Fachada.getInstância().adicionarOfertaDeVenda( 10.53, 11, "Banese-ON53" );
      }
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
            JanelaDoHomebroker.this.enviarCommando( JanelaDoHomebroker.this.conteúdo() );
         }
      } );
      this.botãoDeComandos.setPreferredSize( new Dimension( 200, 35 ) );
      this.botãoDeComandos.setFocusable( false );
   }
   
   private void configurarBotãoDeOfertas()
   {
      final JanelaDeOfertas JANELA_DE_OFERTAS = JanelaDoHomebroker.JANELA_DE_OFERTAS;
      
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
            Fachada.getInstância().adicionarOfertaDeVenda( 10.53, 11, "Banese-ON53" );
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
            Fachada.getInstância().adicionarOfertaDeCompra( 12.63, 3, "Banese-ON53" );
         }
      } );
      this.botãoDeTeste2.setPreferredSize( new Dimension( 270, 35 ) );
      this.botãoDeTeste2.setFocusable( false );
   }
   
   private void configurarBotãoDeVendas()
   {
      final JanelaDeVendas JANELA_DE_VENDAS = JanelaDoHomebroker.JANELA_DE_VENDAS;
      
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
   
   private void configurarCampoDeAjuda()
   {
      if( this.fachada.isAdministradora() )
      {
         this.campoDeAjuda =//@formatter:off
            "Bem-vindo ao Simulador de Homebroker.\n"
            + "Digite 'a' para remover os privilégios de administrador de uma conta.\n"
            + "Digite 'ad' para adicionar privilégios de administrador de uma conta.\n"
            + "Digite 'b' para para bloquear uma conta de usuário.\n"
            + "Digite 'c' para para criar uma conta.\n" 
            + "Digite 'ex' para excluir uma conta.\n"
            + "Digite 'i' para ver o inventario.\n"
            + "Digite 'l' para fazer logoff.\n"
            + "Digite 'm' para ver o mercado.\n"
            + "Digite 'mc' para mudar sua senha atual.\n"
            + "Digite 'ms' para mudar a senha de alguma outra conta.\n"
            + "Digite 'ov' para enviar uma ordem de venda.\n"
            + "Digite 'oc' para criar um ordem de compra.\n" 
            + "Digite 'v' para ver as vendas.\n" 
            + "Digite 't' para adicionar ofertas de teste.\n"
            + "Digite 's' para fechar o programa." ;// @formatter:on
      } else
      {
         this.campoDeAjuda =//@formatter:off
            "Bem-vindo ao Simulador de Homebroker.\n"
            + "Digite 'i' para ver o inventario.\n"
            + "Digite 'l' para fazer logoff.\n"
            + "Digite 'm' para ver o mercado.\n"
            + "Digite 'mc' para mudar sua senha atual.\n"
            + "Digite 'ov' para enviar uma ordem de venda.\n"
            + "Digite 'oc' para criar um ordem de compra.\n" 
            + "Digite 'v' para ver as vendas.\n" 
            + "Digite 's' para fechar o programa." ;// @formatter:on
      }
      this.campoDeAjudaTexto = new JTextArea( this.campoDeAjuda );
      
      this.campoDeAjudaTexto.setEditable( false );
      this.campoDeAjudaTexto.setFocusable( false );
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
            JanelaDoHomebroker.this.enviarCommando( ae.getActionCommand() );
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
            JanelaDoHomebroker.this.limpar();
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
               JanelaDoHomebroker.this.limpar();
            }
            this.primeiraVez = false;
         }
      } );
      this.entradaDeComandos.setPreferredSize( new Dimension( 250, 35 ) );
   }
   
   /**
    * 
    */
   private void configurarJanela()
   {
      // Adiciona o painel principal nesta janela
      this.painel.setDoubleBuffered( true );
      this.janela.add( this.painel );
      
      // Define que a janela deve fechar ao sair.
      this.janela.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
      
      if( this.fachada.isAdministradora() )
      {
         this.janela.setLocation( 110, 10 );
         
      } else
      {
         this.janela.setLocation( 250, 10 );
      }
      
      // Abre a janela maximizado
      // this.setExtendedState( Frame.MAXIMIZED_BOTH );
      
      // Ajusta a janela ao tamanho dos elementos.
      this.janela.pack();
      this.janela.setVisible( true );
   }
   
   /**
    * 
    */
   private void configurarPainel()
   {
      // Configura os componentes
      this.configurarEntradaDeComandos();
      this.configurarBotãoDeComandos();
      this.configurarCampoDeAjuda();
      this.configurarPainelDeBotões();
      
      // Adiciona os componentes ao painel principal
      this.painel.add( this.botãoDeComandos, BorderLayout.WEST );
      this.painel.add( this.entradaDeComandos, BorderLayout.NORTH );
      this.painel.add( this.campoDeAjudaTexto, BorderLayout.EAST );
      this.painel.add( this.painelDeBotões, BorderLayout.CENTER );
      
      Biblioteca.trocarFontes( this.painel, new Font( this.painel.getName(), Frame.NORMAL, 22 ) );
      
   }
   
   private void configurarPainelDeBotões()
   {
      this.painelDeBotões = new JPanel( new GridBagLayout() );
      
      this.botãoDeOfertas = new JButton( "Janela de Ofertas" );
      this.botãoDeVendas = new JButton( "Janela de Vendas" );
      
      this.configurarBotãoDeOfertas();
      this.configurarBotãoDeVendas();
      
      final GridBagConstraints gridBagConstraint = new GridBagConstraints();
      final GridBagConstraints separatorConstraint = new GridBagConstraints();
      
      separatorConstraint.gridx = 0;
      separatorConstraint.gridwidth = 1;
      separatorConstraint.weighty = 1;
      separatorConstraint.gridy = 2;
      
      this.painelDeBotões.add( new JSeparator( SwingConstants.HORIZONTAL ), separatorConstraint );
      
      gridBagConstraint.gridx = 0;
      gridBagConstraint.gridwidth = 1;
      
      gridBagConstraint.gridy = 0;
      this.painelDeBotões.add( this.botãoDeOfertas, gridBagConstraint );
      gridBagConstraint.gridy = 1;
      this.painelDeBotões.add( this.botãoDeVendas, gridBagConstraint );
      
      if( this.fachada.isAdministradora() )
      {
         this.botãoDeTeste1 = new JButton( "Adicionar Venda Teste" );
         this.botãoDeTeste2 = new JButton( "Adicionar Compra Teste" );
         
         this.configurarBotãoDeTeste1();
         this.configurarBotãoDeTeste2();
         
         gridBagConstraint.gridy = 3;
         this.painelDeBotões.add( this.botãoDeTeste1, gridBagConstraint );
         gridBagConstraint.gridy = 4;
         this.painelDeBotões.add( this.botãoDeTeste2, gridBagConstraint );
      }
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
         Fachada.sairDoSistema();
         break;
      case "i":
         this.mostrarInventário();
         break;
      case "ad":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.adicionarPrivilégios();
         break;
      case "b":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.efetuarBloqueio();
         break;
      case "c":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.efetuarCadastro();
         break;
      case "ov":
         JanelaDoHomebroker.JANELA_DE_VENDAS.efetuarVenda();
         break;
      case "ex":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.excluirConta();
         break;
      case "oc":
         JanelaDoHomebroker.JANELA_DE_VENDAS.efetuarCompra();
         break;
      case "m":
         JanelaDoHomebroker.JANELA_DE_OFERTAS.setVisible( true );
         break;
      case "mc":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.alterarSenha( null );
         break;
      case "ms":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.alterarSenhas();
         break;
      case "v":
         JanelaDoHomebroker.JANELA_DE_VENDAS.setVisible( true );
         break;
      case "t":
         this.adicionarOfertasTeste();
         break;
      case "a":
         JanelaDoHomebroker.JANELA_DE_CADASTRO.removerPrivilégios();
         break;
      case "l":
         this.fazerReLogin();
         return;
      default:
         this.imputError();
         break;
      }
      this.limpar();
   }
   
   /**
    * 
    */
   private void fazerReLogin()
   {
      JanelaDoHomebroker.JANELA_DE_OFERTAS.setVisible( false );
      JanelaDoHomebroker.JANELA_DE_VENDAS.setVisible( false );
      JanelaDoHomebroker.JANELA_DE_CADASTRO.setVisible( false );
      JanelaDeLogin.getInstância().loginNoSistema( "dica" );
   }
   
   @Override
   protected void finalize()
   {
      try
      {
         this.janela.setVisible( false );
         JanelaDoHomebroker.instância = null;
         
         this.botãoDeTeste1 = null;
         this.botãoDeTeste2 = null;
         this.botãoDeOfertas = null;
         this.botãoDeVendas = null;
         this.painelDeBotões = null;
         this.campoDeAjudaTexto = null;
         this.campoDeAjuda = null;
         this.entradaDeComandos = null;
         this.botãoDeComandos = null;
         this.janela = null;
         this.painel = null;
         
         super.finalize();
         
      } catch( final Throwable exeption )
      {
         exeption.printStackTrace();
      }
   }
   
   private void imputError()
   {
      JOptionPane.showMessageDialog( null, "Você digitou uma " + "opção inválida!\n\n"
         + this.campoDeAjuda );
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
      if( !this.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há " + "nenhuma conta carregada no sistema!" );
         return;
      }
      JOptionPane.showMessageDialog( null, this.fachada.inventarioToString() );
   }
}
