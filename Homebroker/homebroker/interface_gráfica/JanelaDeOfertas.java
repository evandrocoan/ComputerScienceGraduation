/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Toolkit;

import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

import util.Biblioteca;

/**
 * Classe que constrói a interface gráfica do book de ofertas.
 *
 * @author Professional
 */
public final class JanelaDeOfertas
{
   private static final JanelaDeOfertas INSTÂNCIA;
   
   static
   {
      INSTÂNCIA = new JanelaDeOfertas();
   }
   
   private final JFrame janela;
   private final JPanel painel;
   private final Fachada fachada;
   
   private final DefaultListModel< String > modeloPadrãoDeLista;
   private final JList< String > listaDeOfertas;
   
   private JanelaDeOfertas()
   {
      this.fachada = Fachada.getInstância();
      
      this.painel = new JPanel( new GridLayout( 0, 1 ) );
      this.modeloPadrãoDeLista = new DefaultListModel<>();
      this.listaDeOfertas = new JList<>( this.modeloPadrãoDeLista );
      this.configurarPainel();
      
      this.janela = new JFrame( "Book De Ofertas" );
      this.configurarJanela();
   }
   
   public static JanelaDeOfertas getInstância()
   {
      return JanelaDeOfertas.INSTÂNCIA;
   }
   
   public void adicionarOferta( final String ofertaDeMercado )
   {
      this.modeloPadrãoDeLista.addElement( ofertaDeMercado );
   }
   
   /**
    * Atualiza a lista de ofertas do book de ofertas.
    */
   void atualizarListaDeOfertas()
   {
      int indice = this.tamanhoDaLista();
      
      while( true )
      {
         try
         {
            final String ofertaDoMercado = this.fachada.ofertaToString( indice );
            this.adicionarOferta( ofertaDoMercado );
            
         } catch( final Exception e )
         {
            break;
         }
         indice++;
      }
   }
   
   private void configurarJanela()
   {
      final Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
      final int width = (int) tamanhoDaJanela.getWidth();
      final int height = (int) tamanhoDaJanela.getHeight();
      final Dimension tamanhoDaJanelaReduzido = new Dimension( width - 100, height - 100 );
      
      this.janela.setSize( tamanhoDaJanelaReduzido );
      this.janela.setPreferredSize( tamanhoDaJanelaReduzido );
      this.janela.setBounds( 10, 365, width - 540, height - 400 );
      this.janela.setVisible( false );
      this.janela.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
      this.janela.setContentPane( this.painel );
      
      Biblioteca.trocarFontes( this.janela, new Font( this.janela.getName(), Frame.NORMAL, 20 ) );
   }
   
   private void configurarPainel()
   {
      this.painel.setSize( this.painel.getSize() );
      this.painel.setPreferredSize( this.painel.getSize() );
      this.painel.setVisible( true );
      
      final JScrollPane painelRolável = new JScrollPane( this.listaDeOfertas );
      this.painel.add( painelRolável, BorderLayout.CENTER );
   }
   
   public void setVisible( final boolean b )
   {
      this.janela.setVisible( b );
   }
   
   public int tamanhoDaLista()
   {
      return this.modeloPadrãoDeLista.getSize();
   }
}
