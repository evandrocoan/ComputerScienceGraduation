package observer_design_pattern;

public class HexaObservador extends Observador
{
   public HexaObservador( final ObjetoDeInteresse objetoDeInteresse )
   {
      this.objetoDeInteresse = objetoDeInteresse;
      this.objetoDeInteresse.adicionarObservador( this );
   }
   
   @Override
   public void atualizar()
   {
      System.out.println( "Hex String: "
         + Integer.toHexString( this.objetoDeInteresse.getEstado() ).toUpperCase() );
   }
}
