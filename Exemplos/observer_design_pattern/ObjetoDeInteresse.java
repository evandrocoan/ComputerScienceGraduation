package observer_design_pattern;

import java.util.ArrayList;
import java.util.List;

public class ObjetoDeInteresse
{
   /**
    * Mantém uma lista de seus dependentes, chamados de observadores, e notifica-los automaticamente
    * de quaisquer mudanças de estado, geralmente chamando um de seus métodos
    */
   private final List< Observador > observadores = new ArrayList<>();
   
   private int estado;
   
   public void adicionarObservador( final Observador observador )
   {
      this.observadores.add( observador );
   }
   
   public int getEstado()
   {
      return this.estado;
   }
   
   public void notificarTodosOsObservadores()
   {
      for( final Observador observador: this.observadores )
      {
         observador.atualizar();
      }
   }
   
   public void setEstado( final int state )
   {
      this.estado = state;
      this.notificarTodosOsObservadores();
   }
}
