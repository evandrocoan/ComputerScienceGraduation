package observer_design_pattern;

public class BinárioObservador extends Observador
{
   public BinárioObservador( final ObjetoDeInteresse objetoDeInteresse )
   {
      this.objetoDeInteresse = objetoDeInteresse;
      this.objetoDeInteresse.adicionarObservador( this );
   }
   
   @Override
   public void atualizar()
   {
      System.out.println( "Binário String: "
         + Integer.toBinaryString( this.objetoDeInteresse.getEstado() ) );
   }
}
