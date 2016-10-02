/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho3;

/**
 *
 * @author Professional
 */
public class Relogio 
{
	private int hora;
	private int minuto;
   private int segundo;
   
   private int horaExtra = 0, minutoExtra = 0;
   
	// class's constructor
	public Relogio()
	{
		hora = 0;
		minuto = 0;
      segundo = 0;
		
	}
   
	// class's constructor
	public Relogio( int hora, int minuto, int segundo )
	{
		this.hora = hora;
		this.minuto = minuto;
      this.segundo = segundo;
      
      correctSegundo();
      
	}
   
   /****************** ACCESS METHODS 'SETTERS' ******************/
   /**
    * @param hora the hora to set
    */
   public void setHora(int hora) 
   {
      this.hora = hora;
      
      correctHora();
      
   }

   /**
    * @param minuto the minuto to set
    */
   public void setMinuto(int minuto) 
   {
      this.minuto = minuto;
      
      correctMinuto();
      
   }

   /**
    * @param segundo the segundo to set
    */
   public void setSegundo(int segundo) 
   {
      this.segundo = segundo;
      
      correctSegundo();
      
   }
   
   /****************** ACCESS METHODS 'GETTERS' ******************/
   /**
    * @return the hora
    */
   public int getHora() 
   {
      return hora;
      
   }
   
   /**
    * @return the minuto
    */
   public int getMinuto() 
   {
      return minuto;
      
   }
   
   /**
    * @return the segundo
    */
   public int getSegundo() 
   {
      return segundo;
      
   }
   
   /****************** CORRECT METHODS ******************/
   private void correctSegundo()
   {
      while( segundo > 59 )
      {
         segundo -= 60;
         
         minutoExtra++;
         
      }
      
      updateMinuto();
      
   }
   
   private void correctMinuto()
   {
      while( minuto > 59 )
      {
         minuto -= 60;
         
         horaExtra++;
         
      }
      
      updateHora();
      
   }
   
   private void correctHora()
   {
      while( hora > 23 )
      {
         hora -= 24;
         
      }
      
   }
   
   
   /****************** UPDATE METHODS ******************/
   private void updateMinuto()
   {
      minuto = minuto + minutoExtra;
      minutoExtra = 0; 
      
      correctMinuto();
      
   }
   
   private void updateHora()
   {
      hora = hora + horaExtra;
      horaExtra = 0;
      
      correctHora();
      
   }
   
   /****************** PUBLIC METHODS ******************/
	public String fornecaHorario()
   {
      String text = String.format( "A hora é: %d:%d:%d ", 
              hora, minuto, segundo );
      
      return text;
      
   }
   
   public void tictac()
   {
      segundo++;
      
      correctSegundo();
      
   }
   
} // end class Relogio
