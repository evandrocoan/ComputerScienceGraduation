package observer_design_pattern;

public class OctaObservador extends Observador
{
   public OctaObservador( final ObjetoDeInteresse objetoDeInteresse )
   {
      this.objetoDeInteresse = objetoDeInteresse;
      this.objetoDeInteresse.adicionarObservador( this );
   }
   
   @Override
   public void atualizar()
   {
      System.out.println( "Octa String: "
         + Integer.toOctalString( this.objetoDeInteresse.getEstado() ) );
   }
}
