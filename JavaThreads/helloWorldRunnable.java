public class helloWorldRunnable implements Runnable
{
    private threadID;
    
    public helloWorldRunnable( int id )
    {
        this.threadID = id;
    }
    
    public void run()
    {
        System.out.printf( "Minha id é: %d", this.threadID );
    }
}
