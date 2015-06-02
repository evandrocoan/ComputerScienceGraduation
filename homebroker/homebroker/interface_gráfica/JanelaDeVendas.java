/**
 * 
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 * 
 * @author Professional
 */
public final class JanelaDeVendas extends JFrame
{
   private static JanelaDeVendas instância;
   
   private final MotorDoHomebroker motor;
   
   private JanelaDeVendas( final MotorDoHomebroker motor )
   {
      this.motor = motor;
   }
   
   /**
    * @param motor o motor do Homebroker.
    * @return instância uma instância da janela de login.
    */
   public static JanelaDeVendas getInstância( final MotorDoHomebroker motor )
   {
      synchronized( JanelaDeVendas.class )
      {
         if( JanelaDeVendas.instância == null )
         {
            JanelaDeVendas.instância = new JanelaDeVendas( motor );
         }
      }
      return JanelaDeVendas.instância;
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarCompra()
   {
      if( !this.motor.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há "
                  + "nenhuma conta carregada no sistema!" );
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
      JOptionPane.showMessageDialog( null,
               "Oferta de compra realizada com sucesso!" );
   }
   
   /**
    * Efetua a venda de ações.
    */
   public void efetuarVenda()
   {
      if( !this.motor.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há "
                  + "nenhuma conta carregada no sistema!" );
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
      JOptionPane.showMessageDialog( null,
               "Oferta de venda realizada com sucesso!" );
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
                  JOptionPane.showInputDialog( ( nÉsimaVez
                           ? "Ação não existente!\n\n" : "" )
                           + "Lista de ações disponíveis para venda: \n"
                           + this.motor.inventarioToString() );
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
               JOptionPane
                        .showInputDialog(
                                 "Insira o preço da ação:",
                                 ( modo? Double.toString( this.motor
                                          .getPreço( ação ) ) : "" ) );
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
                  JOptionPane.showInputDialog(
                           ( nÉsimaVez? "Quantidade não existente!\n\n" : "" )
                                    + "Insira a quantidade da ação:",
                           ( modo? Integer.toString( this.motor
                                    .getQuantidade( ação ) ) : "" ) );
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
}
