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
         final String nome = this.getNomeAção();
         if( nome == null )
         {
            return;
         }
         final double preço = this.getPreçoAção( nome );
         if( preço == 0 )
         {
            return;
         }
         final int quantidade = this.getQuantidadeAção( nome );
         if( quantidade == 0 )
         {
            return;
         }
         sucesso = this.motor.adicionarOfertaDeVenda( preço, quantidade, nome );
      }
      
   }
   
   private String getNomeAção()
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
         sucesso = this.motor.existeNoInventário( açãoParaVender );
         nÉsimaVez = true;
      }
      return açãoParaVender;
   }
   
   private double getPreçoAção( final String açãoParaVender )
   {
      final String imput =
               JOptionPane
                        .showInputDialog( "Insira o preço da ação:", Double
                                 .toString( this.motor
                                          .getPreço( açãoParaVender ) ) );
      if( imput == null )
      {
         return 0;
      }
      double preço;
      
      preço = Double.parseDouble( imput );
      return preço;
   }
   
   private int getQuantidadeAção( final String açãoParaVender )
   {
      boolean sucesso = false;
      boolean nÉsimaVez = false;
      int quantidade = 0;
      
      while( !sucesso )
      {
         final String imput =
                  JOptionPane.showInputDialog( ( nÉsimaVez
                           ? "Quantidade não existente!\n\n" : "" )
                           + "Insira a quantidade da ação:",
                           Integer.toString( this.motor
                                    .getQuantidade( açãoParaVender ) ) );
         if( imput == null )
         {
            return 0;
         }
         quantidade = (int) Double.parseDouble( imput );
         sucesso = this.motor.existeQuantidade( quantidade );
         nÉsimaVez = true;
      }
      return quantidade;
   }
}
