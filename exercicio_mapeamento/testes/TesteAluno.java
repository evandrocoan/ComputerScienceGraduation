package testes;

import modelo.Universidade;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TesteAluno
{
    
    Universidade universidade = new Universidade();
    
    @Test
    public void cadastra1AlunoComCurso()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 01 );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
    }
    
    @Test
    public void cadastra1AlunoExistenteComNovoCurso()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 01 );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 02 );
        Assert.assertNotSame( "CCO",
            universidade.retornaAluno( 1001 ).retornaCurso().retornaNome() );
        Assert.assertNotSame( "SIN",
            universidade.retornaAluno( 1001 ).retornaCurso().retornaNome() );
    }
    
    @Test
    public void cadastra1AlunoSemCurso()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade" );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        Assert.assertNull( universidade.retornaAluno( 1001 ).retornaCurso() );
    }
    
    @Test
    public void cadastra2AlunosComMesmoCurso()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 01 );
        universidade.cadastraAluno( 1005, "Maria", "Trindade", 01 );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        Assert.assertEquals( "CCO",
            universidade.retornaAluno( 1001 ).retornaCurso().retornaNome() );
        Assert.assertEquals( "Maria",
            universidade.retornaAluno( 1005 ).retornaNome() );
        Assert.assertEquals( "CCO",
            universidade.retornaAluno( 1005 ).retornaCurso().retornaNome() );
    }
    
    @Test
    public void cadastra2AlunosSemCurso()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade" );
        universidade.cadastraAluno( 1005, "Maria", "Trindade" );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        Assert.assertEquals( "Maria",
            universidade.retornaAluno( 1005 ).retornaNome() );
    }
    
    @Test
    public void cadastra3AlunosComCursosDiferentes()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 01 );
        universidade.cadastraAluno( 1005, "Maria", "Trindade", 02 );
        universidade.cadastraAluno( 1007, "Zezinho", "Estreito", 01 );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        Assert.assertEquals( "CCO",
            universidade.retornaAluno( 1001 ).retornaCurso().retornaNome() );
        Assert.assertEquals( "Maria",
            universidade.retornaAluno( 1005 ).retornaNome() );
        Assert.assertEquals( "SIN",
            universidade.retornaAluno( 1005 ).retornaCurso().retornaNome() );
        Assert.assertEquals( "Zezinho",
            universidade.retornaAluno( 1007 ).retornaNome() );
        Assert.assertEquals( "CCO",
            universidade.retornaAluno( 1007 ).retornaCurso().retornaNome() );
    }
    
    @Test
    public void remove1AlunoCadastrado()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 01 );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        universidade.removeAluno( 1001 );
        Assert.assertNull( universidade.retornaAluno( 1001 ) );
    }
    
    @Test
    public void remove2AlunosCadastrados()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraAluno( 1001, "Antonio", "Trindade", 01 );
        universidade.cadastraAluno( 1005, "Maria", "Trindade", 02 );
        universidade.cadastraAluno( 1007, "Zezinho", "Estreito", 01 );
        Assert.assertEquals( "Antonio",
            universidade.retornaAluno( 1001 ).retornaNome() );
        Assert.assertEquals( "Maria",
            universidade.retornaAluno( 1005 ).retornaNome() );
        Assert.assertEquals( "Zezinho",
            universidade.retornaAluno( 1007 ).retornaNome() );
        universidade.removeAluno( 1001 );
        universidade.removeAluno( 1007 );
        Assert.assertNull( universidade.retornaAluno( 1001 ) );
        Assert.assertEquals( "Maria",
            universidade.retornaAluno( 1005 ).retornaNome() );
        Assert.assertNull( universidade.retornaAluno( 1007 ) );
    }
    
    @Before
    public void removeCursosDaTabelaCurso()
    {
        this.universidade.apagaTodosDadosDoBD();
        this.universidade.cadastraCurso( 01, "CCO", "CTC" );
        this.universidade.cadastraCurso( 02, "SIN", "CTC" );
        this.universidade.cadastraCurso( 03, "ECA", "CTC" );
        this.universidade.cadastraCurso( 04, "EPS", "CTC" );
    }
    
}
