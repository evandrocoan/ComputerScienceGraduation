package mapeadores;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import modelo.Aluno;
import modelo.Curso;

@SuppressWarnings( "resource" )
public class MapeadorAluno
{
    
    private final Connection con;
    private MapeadorCurso mapeadorCurso;
    
    public MapeadorAluno( final Connection con )
    {
        this.con = con;
        this.mapeadorCurso = new MapeadorCurso( con );
    }
    
    private void atualizaAlunoExistente( final Aluno aluno )
        throws SQLException
    {
        final PreparedStatement stmt = this.con.prepareStatement(
            "UPDATE ALUNO SET NOME=?,ENDERECO=?,"
                +
                "CURSO=? WHERE MATRICULA=?" );
        try
        {
            
            stmt.setString( 1, aluno.retornaNome() );
            stmt.setString( 2, aluno.retornaEndereco() );
            if( aluno.retornaCurso() == null )
            {
                stmt.setNull( 3, Types.INTEGER );
            } else
            {
                stmt.setInt( 3, aluno.retornaCurso().retornaCodigo() );
            }
            stmt.setInt( 4, aluno.retornaMatricula() );
            stmt.execute();
        } finally
        {
            stmt.close();
        }
    }
    
    public Aluno get( final int matricula ) throws SQLException
    {
        Aluno aluno;
        final PreparedStatement stmt = this.con.prepareStatement(
            "SELECT NOME,ENDERECO,CURSO "
                +
            "FROM ALUNO WHERE MATRICULA=?" );
        stmt.setInt( 1, matricula );
        final ResultSet rs = stmt.executeQuery();
        try
        {
            if( rs.next() )
            {
                final String nome = rs.getString( "NOME" );
                final String endereco = rs.getString( "ENDERECO" );
                this.mapeadorCurso = new MapeadorCurso( this.con );
                final Curso curso = this.mapeadorCurso.get( rs.getInt(
                    "CURSO" ) );
                aluno = new Aluno( matricula, nome );
                aluno.atribuiEndereco( endereco );
                aluno.atribuiCurso( curso );
                return aluno;
            } else
            {
                return null;
            }
        } finally
        {
            rs.close();
            stmt.close();
        }
    }
    
    private void insereNovoAluno( final Aluno aluno ) throws SQLException
    {
        final PreparedStatement stmt = this.con.prepareStatement(
            "INSERT INTO ALUNO (MATRICULA,NOME,ENDERECO,CURSO) "
                +
                "VALUES (?,?,?,?)" );
        try
        {
            stmt.setInt( 1, aluno.retornaMatricula() );
            stmt.setString( 2, aluno.retornaNome() );
            stmt.setString( 3, aluno.retornaEndereco() );
            if( aluno.retornaCurso() == null )
            {
                stmt.setNull( 4, Types.INTEGER );
            } else
            {
                stmt.setInt( 4, aluno.retornaCurso().retornaCodigo() );
            }
            stmt.execute();
        } finally
        {
            stmt.close();
        }
    }
    
    public void put( final Aluno aluno ) throws SQLException
    {
        if( this.get( aluno.retornaMatricula() ) != null )
        {
            this.atualizaAlunoExistente( aluno );
        }
        else
        {
            this.insereNovoAluno( aluno );
        }
    }
    
    public void remove( final int matricula ) throws SQLException
    {
        final PreparedStatement stmt = this.con.prepareStatement(
            "DELETE FROM ALUNO WHERE MATRICULA=?" );
        stmt.setInt( 1, matricula );
        stmt.execute();
        stmt.close();
    }
    
}
