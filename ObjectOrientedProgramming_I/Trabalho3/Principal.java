/*
 * Escreva uma clase de forma que toda instância dessa 
 * classe represente um relógio. Escreva um método 
 * main conforme a seguir para testar a sua classe.
 */

package Trabalho3;

/**
 *
 * @author Professional
 */
public class Principal 
{
	// main method begins execution Java application
	public static void main( String[] args )
	{
		Interface aInterface = new Interface();
      
      // r representa um relogio que marca 0:0:0
      Relogio r = new Relogio();
      
      Relogio d = new Relogio( 11, 59, 59 );
      
      // horário = 11:59:59
      String horario = d.fornecaHorario();
      
      d.tictac(); // incrementa horário em 1 segundo
      d.tictac();
      
      // será mostrado 12:0:1
      aInterface.mostreMensagem( d.fornecaHorario() );
      
      Relogio rel = new Relogio( 23, 59, 59 );
      rel.tictac();
      
      // será mostrado 0:0:0
      aInterface.mostreMensagem( rel.fornecaHorario() );
		
	} // end main method
	
} // end class Principal
