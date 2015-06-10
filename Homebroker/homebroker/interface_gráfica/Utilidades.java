/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.Fachada;

import javax.swing.JOptionPane;

/**
 * Janela principal que contém o programa e inicia a execução do Homebroker.
 *
 * @author Professional
 */
public final class Utilidades
{
   private final static Fachada fachada = Fachada.getInstância();
   
   /**
    * Construtor que cria a janela principal do programa.
    */
   private Utilidades()
   {
   }
   
   /**
    * @return true caso esteja logado no sistema uma conta de administrador.
    */
   public static boolean isAdministradora()
   {
      if( !Utilidades.fachada.isAutenticada() )
      {
         JOptionPane.showMessageDialog( null, "Não há nenhuma conta carregada no sistema!" );
         return false;
      }
      if( !Utilidades.fachada.isAdministradora() )
      {
         JOptionPane.showMessageDialog( null, "Acesso negado! "
            + "Você precisa ter privilégio de administrador." );
         return false;
      }
      return true;
   }
}
