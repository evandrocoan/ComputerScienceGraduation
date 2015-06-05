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
public class Atualizador implements Runnable
{
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
         MotorDoHomebroker.getInstância().adicionarOfertaDeCompra( 10, 3, "Tabajara SAS" );
         MotorDoHomebroker.getInstância().adicionarOfertaDeVenda( 10, 10, "Tabajara SAS" );
         
         SwingUtilities.invokeLater( new Runnable()
         {
            @Override
            public void run()
            {
               JanelaDeVendas.getInstância().atualizarListaDeVendas();
               JanelaDeOfertas.getInstância().atualizarListaDeOfertas();
            }
         } );
         
         try
         {
            Thread.sleep( 5000 );
         } catch( final InterruptedException e )
         {
            e.printStackTrace();
         }
      }
   }
   
}
