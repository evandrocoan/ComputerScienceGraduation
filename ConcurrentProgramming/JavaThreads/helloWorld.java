import java.lang.Thread;

public class helloWorld extends Thread
{
	private int threadID;
	public 	helloWorld( int id )
	{
		this.threadID = id;
	}
	
	//código executado pela thread 
	public void run()
	{
		System.out.printf( "Este é o meu ID: %d\n", threadID  );
	}
}