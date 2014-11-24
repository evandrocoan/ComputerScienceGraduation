public class main
{
	public static void main( String[] args )
	{
		helloWorld hellos = new helloWorld( 1 );
		hellos.start();
		
		helloWorld hellos2 = new helloWorld( 2 );
		hellos2.start();
		
		try{ hellos.join(); } catch ( Exception e ) { }
		try{ hellos2.join(); } catch ( Exception e ) { }
	}
}