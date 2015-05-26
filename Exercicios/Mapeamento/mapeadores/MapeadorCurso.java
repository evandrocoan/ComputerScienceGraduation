/**
 * 
 */
package mapeadores;

import java.sql.Connection;
import java.sql.SQLException;

import modelo.Curso;

/**
 * 
 * @author Professional
 */
@SuppressWarnings( "static-method" )
public class MapeadorCurso
{
    
    public MapeadorCurso( final Connection con )
    {
        // TODO Auto-generated constructor stub
    }
    
    public Curso get( final int int1 ) throws SQLException
    {
        // TODO Auto-generated method stub
        if( int1 > 1 )
        {
            throw new SQLException();
        }
        return null;
    }
    
    public void put( final Curso curso ) throws SQLException
    {
        // TODO Auto-generated method stub
        throw new SQLException();
    }
    
    public void remove( final int codigo ) throws SQLException
    {
        // TODO Auto-generated method stub
        throw new SQLException();
    }
    
    public static void main( final String[] args )
    {
        // TODO Auto-generated method stub
        
    }
    
}
