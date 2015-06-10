package homebroker.lógica_de_execução;

import java.util.ArrayList;
import java.util.List;

public final class ObjetoDeInteresse
{
   private final List< Observador > observadores = new ArrayList<>();
   
   public void adicionarObservador( final Observador observador )
   {
      this.observadores.add( observador );
   }
   
   public void notificarTodosOsObservadores()
   {
      for( final Observador observador: this.observadores )
      {
         observador.atualizar();
      }
   }
}
