/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import javax.swing.SwingUtilities;

/**
 * 
 * @author Professional
 */
public final class Atualizador implements Runnable
{
   /**
    * Implementa uma thread que atualiza o book de ofertas em intervalos de 1000 milisegundos caso
    * haja mudanças.
    *
    * @see java.lang.Runnable#run()
    */
   @Override
   public void run()
   {
      while( true )
      {
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
            Thread.sleep( 1000 );
         } catch( final InterruptedException e )
         {
            e.printStackTrace();
         }
      }
   }
   
}
