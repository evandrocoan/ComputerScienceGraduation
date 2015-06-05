/**
 *   
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

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
public final class JanelaDeVendas extends JFrame implements Runnable
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser
    * instanciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instanciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( JanelaDeVendas.class.getName() );
   
   private static JanelaDeVendas instância;
   private final MotorDoHomebroker motor;
   private final PainelDaJanelaDeVendas painelPrincipal;
   
   private JanelaDeVendas()
   {
      super( "Book De Vendas" );
      
      JanelaDeVendas.LOG.setLevel( Level.OFF );
      this.painelPrincipal = PainelDaJanelaDeVendas.getInstância();
      this.motor = MotorDoHomebroker.getInstância();
      
      this.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE );
      
      final Dimension tamanhoDaJanela = Toolkit.getDefaultToolkit().getScreenSize();
      final int width = (int) tamanhoDaJanela.getWidth();
      final int height = (int) tamanhoDaJanela.getHeight();
      
      final Dimension tamanhoDaJanelaReduzido = new Dimension( width - 100, height - 100 );
      
      this.setSize( tamanhoDaJanelaReduzido );
      this.setPreferredSize( tamanhoDaJanelaReduzido );
      this.setBounds( 50, 50, width - 100, height - 100 );
      this.setVisible( false );
      this.setContentPane( this.painelPrincipal );
      
      Biblioteca.trocarFontes( this, new Font( this.getName(), Frame.NORMAL, 24 ) );
   }
   
   /**
    * @return instância uma instância da janela de login.
    */
   public static JanelaDeVendas getInstância()
   {
      synchronized( JanelaDeVendas.class )
      {
         if( JanelaDeVendas.instância == null )
         {
            JanelaDeVendas.instância = new JanelaDeVendas();
         }
      }
      return JanelaDeVendas.instância;
   }
   
   /**
    * Atualiza a lista de ofertas do book de ofertas.
    */
   private void atualizarListaDeVendas()
   {
      int indice = this.painelPrincipal.tamanhoDaLista();
      
      while( true )
      {
         try
         {
            final String vendaDoMercado = this.motor.vendaToString( indice );
            this.painelPrincipal.adicionarVenda( vendaDoMercado );
            
            if( JanelaDeVendas.LOG.isLoggable( Level.SEVERE ) )
            {
               JanelaDeVendas.LOG.severe( vendaDoMercado );
            }
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
      if( !this.motor.isAutenticada() )
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
         sucesso = this.motor.adicionarOfertaDeCompra( preço, quantidade, nome );
      }
      JOptionPane.showMessageDialog( null, "Oferta de compra realizada com sucesso!" );
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarVenda()
   {
      if( !this.motor.isAutenticada() )
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
         sucesso = this.motor.adicionarOfertaDeVenda( preço, quantidade, nome );
      }
      JOptionPane.showMessageDialog( null, "Oferta de venda realizada com sucesso!" );
   }
   
   /**
    * Obtém do usuário o nome da ação necessária para efetuar alguma operação
    * sobre seu inventário.
    * 
    * @param ação o nome da ação.
    * @param modo um boolean informando se deve ser verificado a existência da
    *           ação informada no inventário.
    * @return a quantidade.
    */
   private String getNome( final boolean modo )
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      String açãoParaVender = null;
      
      while( !sucesso )
      {
         açãoParaVender =
            JOptionPane.showInputDialog( ( nÉsimaVez? "Ação não existente!\n\n" : "" )
               + "Lista de ações disponíveis para venda: \n" + this.motor.inventarioToString() );
         if( açãoParaVender == null )
         {
            return null;
         }
         sucesso = modo? this.motor.existeNoInventário( açãoParaVender ) : true;
         nÉsimaVez = true;
      }
      return açãoParaVender;
   }
   
   /**
    * Obtém do usuário o preço da ação necessária para efetuar alguma operação
    * sobre seu inventário.
    * 
    * @param ação o nome da ação.
    * @param modo um boolean informando se deve ser verificado a existência da
    *           ação informada no inventário.
    * @return preço o preço da ação.
    */
   private double getPreço( final String ação, final boolean modo )
   {
      final String imput =
         JOptionPane.showInputDialog( "Insira o preço da ação:",
            ( modo? Double.toString( this.motor.getPreço( ação ) ) : "" ) );
      if( imput == null )
      {
         return 0;
      }
      double preço;
      
      preço = Double.parseDouble( imput );
      return preço;
   }
   
   /**
    * Obtém do usuário a quantidade de ações necessária para efetuar alguma
    * operação sobre seu inventário.
    * 
    * @param ação o nome da ação.
    * @param modo um boolean informando se deve ser verificado a existência da
    *           ação informada no inventário.
    * @return a quantidade.
    */
   private int getQuantidade( final String ação, final boolean modo )
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int quantidade = 0;
      
      while( !sucesso )
      {
         final String imput =
            JOptionPane.showInputDialog( ( nÉsimaVez? "Quantidade não existente!\n\n" : "" )
               + "Insira a quantidade da ação:",
               ( modo? Integer.toString( this.motor.getQuantidade( ação ) ) : "" ) );
         if( imput == null )
         {
            return 0;
         }
         quantidade = (int) Double.parseDouble( imput );
         sucesso = modo? this.motor.existeQuantidade( quantidade, ação ) : true;
         nÉsimaVez = true;
      }
      return quantidade;
   }
   
   /**
    * Implementa uma thread que atualiza o book de ofertas em intervalos de 1000
    * milisegundos caso haja mudanças.
    *
    * @see java.lang.Runnable#run()
    */
   @Override
   public void run()
   {
      while( true )
      {
         if( JanelaDeVendas.LOG.isLoggable( Level.SEVERE ) )
         {
            final String texto =
               "Estou em JanelaDoBook chamando o teste "
                  + "\n\n this.bookDeOfertas.existemNovasOfertas( "
                  + "this.janelaDoBook.getNúmeroDeOfertas()"
                  + this.motor.existemNovasOfertas( this.painelPrincipal.tamanhoDaLista() );
            JanelaDeVendas.LOG.severe( texto );
         }
         
         this.motor.adicionarOfertaDeCompra( 10, 5, "Tabajara SAS" );
         
         if( this.motor.existemNovasOfertas( this.painelPrincipal.tamanhoDaLista() ) )
         {
            this.atualizarListaDeVendas();
         }
         try
         {
            Thread.sleep( 1000 );
         } catch( final InterruptedException e )
         {
            e.printStackTrace();
         }
      }
   }
}
