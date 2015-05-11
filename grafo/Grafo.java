package grafo;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Representa um Grafo. Implementa as ações básicas descritas em
 * http://www.inf.ufsc.br/grafos/represen/algoritmos/grafo.html.
 * 
 * Esta implementação pode ser feita em qualquer linguagem de programação, mas
 * deve explicitamente apresentar a estrutura de grafos. Por exemplo, se for
 * feita numa linguagem de programação orientada a objetos deve existir uma
 * classe Grafo.
 * 
 * Os critérios de avaliação incluem:
 * 
 * estrutura do código; legibilidade do código; eficácia e eficiência (esperada
 * complexidade O(1) para as operações básicas)
 * 
 * @author Evandro  Coan
 */
public class Grafo
{
    private static final Logger LOG = Logger.getLogger( Grafo.class.getName() );
    
    /**
     * Serve para armazenar os vértices do grafo e suas arestas.
     */
    private final Hashtable< Object, Hashtable< Object, Object >> vértices;
    
    /**
     * Prepara a estrutura para ser utilizada como um grafo. Antes de ser criado
     * um grafo, este construtor precisa ser chamado.
     */
    public Grafo()
    {
        Grafo.LOG.setLevel( Level.OFF );
        this.vértices = new Hashtable<>();
    }
    
    /**
     * Cria um grafo contendo os vértices passados como parâmetros.
     * 
     * @param vértices um array de vértices.
     * @throws ExeçãoVérticeJáExistente caso já exista um vértice passado como
     *             parametro no grafo.
     */
    public Grafo( final Object vértices ) throws ExeçãoVérticeJáExistente
    {
        this(); // chama o construtor que prepara a estrutura
        this.adicionaVértice( vértices );
    }
    
    /**
     * Adiciona vários novos vértices em G.
     * 
     * @param vértices uma enumeração de vértices para se adicionar.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void adicionaVértice( final Enumeration< ? > vértices )
        throws ExeçãoVérticeJáExistente
    {
        if( this.vértices.containsKey( vértices ) )
        {
            throw new ExeçãoVérticeJáExistente( vértices, this );
        }
        while( vértices.hasMoreElements() )
        {
            this.adicionaVértice( vértices.nextElement() );
        }
    }
    
    /**
     * Adiciona um novo vértice em G.
     * 
     * @param vértice um vértice.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void adicionaVértice( final Object vértice )
        throws ExeçãoVérticeJáExistente
    {
        if( this.vértices.containsKey( vértice ) )
        {
            throw new ExeçãoVérticeJáExistente( vértice, this );
        }
        this.vértices.put( vértice, new Hashtable<>() );
    }
    
    /**
     * Adiciona um novo vértice em G, conectado a vários vértices.
     * 
     * @param vértice um vértice.
     * @param adjacentes uma enumeração de vértices adjacentes.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void adicionaVértice( final Object vértice,
        final Enumeration< ? > adjacentes ) throws ExeçãoVérticeNãoExistente,
        ExeçãoVérticeJáExistente
    {
        this.adicionaVértice( vértice );
        
        while( adjacentes.hasMoreElements() )
        {
            this.conecta( vértice, adjacentes.nextElement() );
        }
    }
    
    /**
     * Adiciona um novo vértice em G, conectado a vários vértices.
     * 
     * @param vértice um vértice.
     * @param adjacentes um arranjo de vértices adjacentes.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void
    adicionaVértice( final Object vértice, final Object[] adjacentes )
        throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
    {
        this.adicionaVértice( vértice );
        
        for( int índice = 0; índice < adjacentes.length; índice++ )
        {
            this.conecta( vértice, adjacentes[índice] );
        }
    }
    
    /**
     * Adiciona vários novos vértices em G.
     * 
     * @param vértices um arranjo de vértices.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void adicionaVértice( final Object[] vértices )
        throws ExeçãoVérticeJáExistente
    {
        if( this.vértices.containsKey( vértices ) )
        {
            throw new ExeçãoVérticeJáExistente( vértices, this );
        }
        for( int índice = 0; índice < vértices.length; índice++ )
        {
            this.adicionaVértice( vértices[índice] );
        }
    }
    
    /**
     * Retorna os vértices adjacentes de um dado vértice.
     * 
     * @param vértice um vértice.
     * @return os vértices como uma enumeração.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public Enumeration< ? > adjacentes( final Object vértice )
        throws ExeçãoVérticeNãoExistente
        {
        if( !this.vértices.containsKey( vértice ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice, this );
        }
        final Hashtable< ?, ? > arestas = this.vértices.get( vértice );
        
        return arestas.elements();
        }
    
    /**
     * Retorna os vértices adjacentes de um dado vértice.
     * 
     * @param vértice um vértice do grafo.
     * @return os vértices com um array.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public Object[] adjacentesEmArranjo( final Object vértice )
        throws ExeçãoVérticeNãoExistente
    {
        final Enumeration< ? > enumeração = this.adjacentes( vértice );
        final Object[] adjacentes = new Object[this.grau( vértice )];
        
        int índice = 0;
        while( enumeração.hasMoreElements() )
        {
            adjacentes[índice] = enumeração.nextElement();
            índice++;
        }
        return adjacentes;
    }
    
    /**
     * Conecta os vértices v1 em uma enumeração de vértices em G.
     * 
     * @param vértice1 o primeiro vértice a conectar.
     * @param vértices uma enumeração de vértices para conectar.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public void
    conecta( final Object vértice1, final Enumeration< ? > vértices )
        throws ExeçãoVérticeNãoExistente
    {
        while( vértices.hasMoreElements() )
        {
            this.conecta( vértice1, vértices.nextElement() );
        }
    }
    
    /**
     * Conecta os vértices v1 e v2 em G.
     * 
     * @param vértice1 o primeiro vértice a conectar.
     * @param vértice2 o segundo vértice a conectar.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public void conecta( final Object vértice1, final Object vértice2 )
        throws ExeçãoVérticeNãoExistente
    {
        if( !this.vértices.containsKey( vértice1 ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice1, this );
        }
        if( !this.vértices.containsKey( vértice2 ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice2, this );
        }
        // pega a chave do vértice
        final Integer chaveDoVértice1 = Integer.valueOf( vértice1.hashCode() );
        final Integer chaveDoVértice2 = Integer.valueOf( vértice2.hashCode() );
        
        // pega a Hashtable de arestas do vértice
        final Hashtable< Object, Object > arestasDoVértice1 =
            this.vértices.get( vértice1 );
        final Hashtable< Object, Object > arestasDoVértice2 =
            this.vértices.get( vértice2 );
        
        // conecta o vértice1 com o vértice2
        arestasDoVértice1.put( chaveDoVértice2, vértice2 );
        arestasDoVértice2.put( chaveDoVértice1, vértice1 );
    }
    
    /**
     * Conecta os vértices v1 em um arranjo de vértices em G.
     * 
     * @param vértice1 o primeiro vértice a conectar.
     * @param vértices um array de vértices para conectar.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public void conecta( final Object vértice1, final Object[] vértices )
        throws ExeçãoVérticeNãoExistente
    {
        for( int índice = 0; índice < vértices.length; índice++ )
        {
            this.conecta( vértice1, vértices[índice] );
        }
    }
    
    /**
     * Informa se um dado vertice existe nesse grafo.
     * 
     * @param vértice um vértice.
     * @return true se existe, false caso contrário.
     */
    public boolean contémVertice( final Object vértice )
    {
        return this.vértices.containsKey( vértice );
    }
    
    /**
     * Desconecta dois vértices.
     * 
     * @param vértice1 o primeiro vértice.
     * @param vértice2 o segundo vértice.
     * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
     */
    public void desconecta( final Object vértice1, final Object vértice2 )
        throws ExeçãoVérticeNãoExistente
    {
        if( !this.vértices.containsKey( vértice1 ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice1, this );
        }
        if( !this.vértices.containsKey( vértice2 ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice2, this );
        }
        // pega a chave do vértice
        final Integer chaveDoVértice1 = Integer.valueOf( vértice1.hashCode() );
        final Integer chaveDoVértice2 = Integer.valueOf( vértice2.hashCode() );
        
        // pega a Hashtable de arestas do vértice
        final Hashtable< Object, Object > arestasDoVértice1 =
            this.vértices.get( vértice1 );
        final Hashtable< Object, Object > arestasDoVértice2 =
            this.vértices.get( vértice2 );
        
        // desconecta o vértice1 do vértice2
        arestasDoVértice1.remove( chaveDoVértice2 );
        arestasDoVértice2.remove( chaveDoVértice1 );
    }
    
    /**
     * Verifica se dois vértices estão conectados.
     * 
     * @param vértice1 o primeiro vértice.
     * @param vértice2 o segundo vértice.
     * @return true se conectados, false caso contrário.
     * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
     */
    public boolean
    estãoConectados( final Object vértice1, final Object vértice2 )
        throws ExeçãoVérticeNãoExistente
    {
        if( !this.vértices.containsKey( vértice1 ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice1, this );
        }
        if( !this.vértices.containsKey( vértice2 ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice2, this );
        }
        // os adjacentes dele
        final Hashtable< ?, ? > adjacentes = this.vértices.get( vértice1 );
        
        final Integer chaveDoVértice2 = Integer.valueOf( vértice2.hashCode() );
        
        return adjacentes.containsKey( chaveDoVértice2 );
    }
    
    /**
     * Retorna o número de vértices adjacentes a v em G.
     * 
     * @param vértice um vértice.
     * @return um inteiro.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public int grau( final Object vértice ) throws ExeçãoVérticeNãoExistente
    {
        if( !this.vértices.containsKey( vértice ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice, this );
        }
        final Hashtable< ?, ? > arestas = this.vértices.get( vértice );
        
        int size = 0;
        
        size = size + arestas.size();
        
        return size;
    }
    
    /**
     * Retorna o número de vértices de G.
     * 
     * @return um inteiro.
     */
    public int ordem()
    {
        return this.vértices.size();
    }
    
    /**
     * Remove um vértice de G, juntamente com todas as conexões.
     * 
     * @param vértice o vértice do grafo.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     */
    public void removerVértice( final Object vértice )
        throws ExeçãoVérticeNãoExistente
    {
        if( !this.vértices.containsKey( vértice ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice, this );
        }
        final Enumeration< ? > adjacentes = this.adjacentes( vértice );
        
        while( adjacentes.hasMoreElements() )
        {
            this.desconecta( vértice, adjacentes.nextElement() );
        }
        this.vértices.remove( vértice );
    }
    
    /**
     * Informa quanto vértices o grafo possui.
     * 
     * @return resultado
     */
    public int tamanhoDoGrafo()
    {
        return this.vértices.size();
    }
    
    /**
     * Retorna uma representação em string do grafo.
     */
    @Override
    public String toString()
    {
        String grafo = "( ";
        final Enumeration< Object > vérticesDoGrafo = this.vértices();
        
        while( vérticesDoGrafo.hasMoreElements() )
        {
            final Object vértice = vérticesDoGrafo.nextElement();
            grafo += vértice + "(";
            
            Enumeration< ? > adjacentes;
            try
            {
                adjacentes = this.adjacentes( vértice );
                
                while( adjacentes.hasMoreElements() )
                {
                    grafo = grafo + " " + adjacentes.nextElement();
                    
                    if( adjacentes.hasMoreElements() )
                    {
                        grafo += ",";
                    } else
                    {
                        grafo += " ";
                    }
                }
            } catch( final ExeçãoVérticeNãoExistente exeção )
            {
                exeção.printStackTrace();
            }
            grafo += ")";
            
            if( vérticesDoGrafo.hasMoreElements() )
            {
                grafo += ", ";
            }
        }
        grafo += " )";
        return grafo;
    }
    
    /**
     * Retorna um vértice qualquer de G.
     * 
     * @return um vértice.
     */
    public Object umVértice()
    {
        return this.vértices.keys().nextElement();
    }
    
    /**
     * Retorna um conjunto contendo os vértices de G.
     * 
     * @return uma enumeração.
     */
    public Enumeration< Object > vértices()
    {
        return this.vértices.keys();
    }
}
