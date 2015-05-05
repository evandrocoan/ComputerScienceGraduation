package modelo;

public class Curso
{
    
    private final int codigo;
    private String nome;
    private String centro;
    
    public Curso( final int codigo )
    {
        this.codigo = codigo;
    }
    
    public void atribuiCentro( final String centro )
    {
        this.centro = centro;
    }
    
    public void atribuiNome( final String nome )
    {
        this.nome = nome;
    }
    
    public String retornaCentro()
    {
        return this.centro;
    }
    
    public int retornaCodigo()
    {
        return this.codigo;
    }
    
    public String retornaNome()
    {
        return this.nome;
    }
    
}
