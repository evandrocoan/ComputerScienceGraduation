import java.lang.Thread;

public class helloWorld extends Thread
{
	private int threadID;
	public 	helloWorld( int id )
	{
		this.threadID = id;
	}
	
	//c�digo executado pela thread 
	public void run()
	{
		System.out.printf( "Este � o meu ID: %d\n", threadID  );
	}
}