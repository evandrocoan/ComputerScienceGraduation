package observer_design_pattern;

/**
 * @see <a href="http://www.tutorialspoint.com/design_pattern/observer_pattern.htm"> Design Patterns
 *      - Observer Pattern</a>
 * @author Professional
 */
public class PadrãoDeProjetoObservadorDemo
{
   public static void main( final String[] args )
   {
      final ObjetoDeInteresse objetoDeInteresse = new ObjetoDeInteresse();
      
      @SuppressWarnings( "unused" )
      final HexaObservador hexaObservador = new HexaObservador( objetoDeInteresse );
      @SuppressWarnings( "unused" )
      final OctaObservador octaObservador = new OctaObservador( objetoDeInteresse );
      @SuppressWarnings( "unused" )
      final BinárioObservador binárioObservador = new BinárioObservador( objetoDeInteresse );
      
      System.out.println( "Primeira mudança de estado: 15" );
      objetoDeInteresse.setEstado( 15 );
      
      System.out.println( "Segunda mudança de estado: 10" );
      objetoDeInteresse.setEstado( 10 );
   }
}
