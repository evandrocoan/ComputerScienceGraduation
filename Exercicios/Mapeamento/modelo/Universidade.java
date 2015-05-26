package modelo;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import mapeadores.MapeadorAluno;
import mapeadores.MapeadorCurso;

@SuppressWarnings( "resource" )
public class Universidade
{
    
    private Connection con;
    private MapeadorAluno mapeadorAluno;
    private MapeadorCurso mapeadorCurso;
    
    public Universidade()
    {
        try
        {
            // BD Postgres
            Class.forName( "org.postgresql.Driver" );
            this.con = DriverManager.getConnection(
                "jdbc:postgresql://localhost/mapeamento-postgres", "postgres",
                "postgres" );
            this.mapeadorAluno = new MapeadorAluno( this.con );
            this.mapeadorCurso = new MapeadorCurso( this.con );
        } catch( final Exception e )
        {
            e.printStackTrace();
            throw new java.lang.RuntimeException( "erro ao conectar" );
        }
    }
    
    public void apagaTodosDadosDoBD()
    {
        Statement stmt = null;
        try
        {
            stmt = this.con.createStatement();
            stmt.executeUpdate( "DELETE FROM ALUNO" );
            stmt.executeUpdate( "DELETE FROM CURSO" );
        } catch( final SQLException e )
        {
            e.printStackTrace();
            throw new java.lang.RuntimeException(
                "erro ao apagar todos os dados do BD" );
        } finally
        {
            try
            {
                stmt.close();
            } catch( final Exception ignore )
            {
                // TODO
            }
        }
    }
    
    public void cadastraAluno( final int matricula, final String nome,
        final String endereco )
    {
        try
        {
            final Aluno aluno = new Aluno( matricula, nome );
            aluno.atribuiEndereco( endereco );
            this.mapeadorAluno.put( aluno );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
    }
    
    public void cadastraAluno( final int matricula, final String nome,
        final String endereco, final int codigoCurso )
    {
        try
        {
            final Curso curso = this.mapeadorCurso.get( codigoCurso );
            final Aluno aluno = new Aluno( matricula, nome );
            aluno.atribuiEndereco( endereco );
            aluno.atribuiCurso( curso );
            this.mapeadorAluno.put( aluno );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
    }
    
    public void cadastraCurso( final int codigo, final String nome,
        final String centro )
    {
        final Curso curso = new Curso( codigo );
        curso.atribuiNome( nome );
        curso.atribuiCentro( centro );
        try
        {
            this.mapeadorCurso.put( curso );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
    }
    
    public void removeAluno( final int matricula )
    {
        try
        {
            this.mapeadorAluno.remove( matricula );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
    }
    
    public void removeCurso( final int codigo )
    {
        try
        {
            this.mapeadorCurso.remove( codigo );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
        
    }
    
    public Aluno retornaAluno( final int matricula )
    {
        try
        {
            return this.mapeadorAluno.get( matricula );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
        return null;
    }
    
    public Curso retornaCurso( final int codigo )
    {
        try
        {
            return this.mapeadorCurso.get( codigo );
        } catch( final SQLException e )
        {
            e.printStackTrace();
        }
        return null;
    }
    
}
