package testes;

import modelo.Curso;
import modelo.Universidade;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TesteCurso
{
    
    Universidade universidade = new Universidade();
    
    @Test
    public void cadastra1Curso()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraCurso( 01, "CCO", "CTC" );
        Assert.assertEquals( "CCO",
            universidade.retornaCurso( 01 ).retornaNome() );
        Assert.assertEquals( "CTC",
            universidade.retornaCurso( 01 ).retornaCentro() );
    }
    
    @Test
    public void cadastra1CursoExistente()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraCurso( 01, "CCO", "CTC" );
        Assert.assertEquals( "CCO",
            universidade.retornaCurso( 01 ).retornaNome() );
        universidade.cadastraCurso( 01, "CCO", "CCE" );
        Assert.assertNotSame( "CCO",
            universidade.retornaCurso( 01 ).retornaCentro() );
        Assert.assertEquals( "CCE",
            universidade.retornaCurso( 01 ).retornaCentro() );
    }
    
    @Test
    public void cadastra2Cursos()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraCurso( 01, "CCO", "CTC" );
        universidade.cadastraCurso( 02, "SIN", "CTC" );
        Assert.assertEquals( "CCO",
            universidade.retornaCurso( 01 ).retornaNome() );
        Assert.assertEquals( "CTC",
            universidade.retornaCurso( 01 ).retornaCentro() );
        Assert.assertEquals( "SIN",
            universidade.retornaCurso( 02 ).retornaNome() );
        Assert.assertEquals( "CTC",
            universidade.retornaCurso( 02 ).retornaCentro() );
    }
    
    @Test
    public void cadastra2CursosExistentes()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraCurso( 01, "CCO", "CTC" );
        universidade.cadastraCurso( 02, "SIN", "CTC" );
        Assert.assertEquals( "CCO",
            universidade.retornaCurso( 01 ).retornaNome() );
        Assert.assertEquals( "SIN",
            universidade.retornaCurso( 02 ).retornaNome() );
        
        universidade.cadastraCurso( 01, "CCO", "CCE" );
        universidade.cadastraCurso( 02, "SIN", "CCS" );
        
        Assert.assertEquals( "CCE",
            universidade.retornaCurso( 01 ).retornaCentro() );
        Assert.assertEquals( "CCS",
            universidade.retornaCurso( 02 ).retornaCentro() );
    }
    
    @Test
    public void consultaCursoInexistente()
    {
        final Universidade universidade = new Universidade();
        final Curso curso = universidade.retornaCurso( 01 );
        Assert.assertNull( curso );
    }
    
    @Test
    public void remove1CursoCadastrado()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraCurso( 01, "CCO", "CTC" );
        Assert.assertEquals( "CCO",
            universidade.retornaCurso( 01 ).retornaNome() );
        universidade.removeCurso( 01 );
        Assert.assertNull( universidade.retornaCurso( 01 ) );
    }
    
    @Test
    public void remove2CursosCadastrado()
    {
        final Universidade universidade = new Universidade();
        universidade.cadastraCurso( 01, "CCO", "CTC" );
        universidade.cadastraCurso( 02, "SIN", "CTC" );
        Assert.assertEquals( "CCO",
            universidade.retornaCurso( 01 ).retornaNome() );
        Assert.assertEquals( "SIN",
            universidade.retornaCurso( 02 ).retornaNome() );
        
        universidade.removeCurso( 01 );
        Assert.assertNull( universidade.retornaCurso( 01 ) );
        Assert.assertEquals( "SIN",
            universidade.retornaCurso( 02 ).retornaNome() );
        
        universidade.removeCurso( 02 );
        Assert.assertNull( universidade.retornaCurso( 02 ) );
    }
    
    @Before
    public void removeCursosDaTabelaCurso()
    {
        this.universidade.apagaTodosDadosDoBD();
    }
    
}
