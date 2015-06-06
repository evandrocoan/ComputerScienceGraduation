/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

import util.Biblioteca;

/**
 * 
 * @author Professional
 */
public final class JanelaDeVendas extends JFrame
{
   private static final Logger LOG;
   private static JanelaDeVendas INSTÂNCIA;
   
   static
   {
      LOG = Logger.getLogger( JanelaDeVendas.class.getName() );
   }
   
   private final Fachada fachada;
   private final PainelDeVendas painel;
   
   private JanelaDeVendas()
   {
      super( "Book De Vendas" );
      
      JanelaDeVendas.LOG.setLevel( Level.OFF );
      this.painel = PainelDeVendas.getInstância();
      this.fachada = Fachada.getInstância();
      
      this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
      
      final Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
      final int width = (int) tamanhoDaJanela.getWidth();
      final int height = (int) tamanhoDaJanela.getHeight();
      
      final Dimension tamanhoDaJanelaReduzido = new Dimension( width - 100, height - 100 );
      
      this.setSize( tamanhoDaJanelaReduzido );
      this.setPreferredSize( tamanhoDaJanelaReduzido );
      this.setBounds( 640, 365, width - 650, height - 400 );
      this.setVisible( false );
      this.setContentPane( this.painel );
      
      Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL, 18 ) );
   }
   
   /**
    * @return instância uma instância da janela de login.
    */
   public static JanelaDeVendas getInstância()
   {
      synchronized( JanelaDeVendas.class )
      {
         if( JanelaDeVendas.INSTÂNCIA == null )
         {
            JanelaDeVendas.INSTÂNCIA = new JanelaDeVendas();
         }
      }
      return JanelaDeVendas.INSTÂNCIA;
   }
   
   /**
    * Atualiza a lista de ofertas do book de ofertas.
    */
   void atualizarListaDeVendas()
   {
      int indice = this.painel.tamanhoDaLista();
      
      while( true )
      {
         try
         {
            final String vendaDoMercado = this.fachada.vendaToString( indice );
            this.painel.adicionarVenda( vendaDoMercado );
            
         } catch( final Exception e )
         {
            break;
         }
         indice++;
      }
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarCompra()
   {
      if( !this.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há " + "nenhuma conta carregada no sistema!" );
         return;
      }
      boolean sucesso = false;
      
      while( !sucesso )
      {
         final String nome = this.getNome( false );
         if( nome == null )
         {
            return;
         }
         final double preço = this.getPreço( nome, false );
         if( preço == 0 )
         {
            return;
         }
         final int quantidade = this.getQuantidade( nome, false );
         if( quantidade == 0 )
         {
            return;
         }
         sucesso = this.fachada.adicionarOfertaDeCompra( preço, quantidade, nome );
      }
      JOptionPane.showMessageDialog( null, "Oferta de compra realizada com sucesso!" );
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarVenda()
   {
      if( !this.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há " + "nenhuma conta carregada no sistema!" );
         return;
      }
      boolean sucesso = false;
      
      while( !sucesso )
      {
         final String nome = this.getNome( true );
         if( nome == null )
         {
            return;
         }
         final double preço = this.getPreço( nome, true );
         if( preço == 0 )
         {
            return;
         }
         final int quantidade = this.getQuantidade( nome, true );
         if( quantidade == 0 )
         {
            return;
         }
         sucesso = this.fachada.adicionarOfertaDeVenda( preço, quantidade, nome );
      }
      JOptionPane.showMessageDialog( null, "Oferta de venda realizada com sucesso!" );
   }
   
   /**
    * Obtém do usuário o nome da ação necessária para efetuar alguma operação sobre seu inventário.
    * 
    * @param ação o nome da ação.
    * @param modo um boolean informando se deve ser verificado a existência da ação informada no
    *           inventário.
    * @return a quantidade.
    */
   private String getNome( final boolean modo )
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      String açãoParaVender = null;
      
      while( !sucesso )
      {
         açãoParaVender = JOptionPane.showInputDialog( ( nÉsimaVez? "Ação não existente!\n\n" : "" )
            + "Lista de ações disponíveis para venda: \n" + this.fachada.inventarioToString() );
         if( açãoParaVender == null )
         {
            return null;
         }
         sucesso = modo? this.fachada.existeNoInventário( açãoParaVender ) : true;
         nÉsimaVez = true;
      }
      return açãoParaVender;
   }
   
   /**
    * Obtém do usuário o preço da ação necessária para efetuar alguma operação sobre seu inventário.
    * 
    * @param ação o nome da ação.
    * @param modo um boolean informando se deve ser verificado a existência da ação informada no
    *           inventário.
    * @return preço o preço da ação.
    */
   private double getPreço( final String ação, final boolean modo )
   {
      final String imput = JOptionPane.showInputDialog( "Insira o preço da ação:",
         ( modo? Double.toString( this.fachada.getPreço( ação ) ) : "" ) );
      if( imput == null )
      {
         return 0;
      }
      double preço;
      
      preço = Double.parseDouble( imput );
      return preço;
   }
   
   /**
    * Obtém do usuário a quantidade de ações necessária para efetuar alguma operação sobre seu
    * inventário.
    * 
    * @param ação o nome da ação.
    * @param modo um boolean informando se deve ser verificado a existência da ação informada no
    *           inventário.
    * @return a quantidade.
    */
   private int getQuantidade( final String ação, final boolean modo )
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int quantidade = 0;
      
      while( !sucesso )
      {
         final String imput = JOptionPane.showInputDialog( ( nÉsimaVez
            ? "Quantidade não existente!\n\n" : "" ) + "Insira a quantidade da ação:", ( modo
            ? Integer.toString( this.fachada.getQuantidade( ação ) ) : "" ) );
         if( imput == null )
         {
            return 0;
         }
         quantidade = (int) Double.parseDouble( imput );
         sucesso = modo? this.fachada.existeQuantidade( quantidade, ação ) : true;
         nÉsimaVez = true;
      }
      return quantidade;
   }
}
