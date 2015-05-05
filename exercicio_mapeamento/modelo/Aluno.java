package modelo;

public class Aluno
{
    
    private final int matricula;
    private final String nome;
    private String endereco;
    private Curso curso;
    
    public Aluno( final int matricula, final String nome )
    {
        this.nome = nome;
        this.matricula = matricula;
    }
    
    public void atribuiCurso( final Curso curso )
    {
        this.curso = curso;
    }
    
    public void atribuiEndereco( final String endereco )
    {
        this.endereco = endereco;
    }
    
    public Curso retornaCurso()
    {
        return this.curso;
    }
    
    public String retornaEndereco()
    {
        return this.endereco;
    }
    
    public int retornaMatricula()
    {
        return this.matricula;
    }
    
    public String retornaNome()
    {
        return this.nome;
    }
    
}
