package grafo;

import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
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
    public void adicionaVértice( final Object vértice, final Object[] adjacentes )
        throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
    
    {
        this.adicionaVértice( vértice );
        
        for( int índice = 0; índice < adjacentes.length; índice++ )
        {
            this.conecta( vértice, adjacentes[índice] );
        }
    }
    
    /**
     * Adiciona um novo vértice em G, conectado a vários vértices.
     * 
     * @param vértice um vértice.
     * @param adjacentes um vetor de vértices adjacentes.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void adicionaVértice( final Object vértice,
        final Vector< ? > adjacentes ) throws ExeçãoVérticeNãoExistente,
        ExeçãoVérticeJáExistente
    {
        this.adicionaVértice( vértice );
        
        final Iterator< ? > adjacentesIterador = adjacentes.iterator();
        
        while( adjacentesIterador.hasNext() )
        {
            this.conecta( vértice, adjacentesIterador.next() );
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
        for( int índice = 0; índice < vértices.length; índice++ )
        {
            this.adicionaVértice( vértices[índice] );
        }
    }
    
    /**
     * Adiciona vários novos vértices em G.
     * 
     * @param vértices um vetor de vértices.
     * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
     */
    public void adicionaVértice( final Vector< Object > vértices )
        throws ExeçãoVérticeJáExistente
    {
        final Iterator< Object > vérticesIterador = vértices.iterator();
        
        while( vérticesIterador.hasNext() )
        {
            this.adicionaVértice( vérticesIterador.next() );
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
    public void conecta( final Object vértice1, final Enumeration< ? > vértices )
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
     * Conecta todos os vértices do arranjo vértices1 em todos os vértices do
     * arranjo vértice2.
     * 
     * @param vértices1 um array de vértices.
     * @param vértices2 um array de vértices.
     * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
     */
    public void conecta( final Object[] vértices1, final Object[] vértices2 )
        throws ExeçãoVérticeNãoExistente
    {
        for( int índice1 = 0; índice1 < vértices1.length; índice1++ )
        {
            for( int índice2 = 0; índice2 < vértices2.length; índice2++ )
            {
                this.conecta( vértices1[índice1], vértices2[índice2] );
            }
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
     * Verifica se este Grafo é uma árvore, ou seja, verifica se não há ciclos e
     * se este é um grafo conexo.
     * 
     * @return true se é arvore, false caso contrário.
     */
    public boolean éÁrvore()
    {
        // TODO
        return false;
    }
    
    /**
     * Verifica se cada vértice de G está conectados a todos os outros vértices.
     * 
     * @return true caso este grafo seja completo, false caso contrário.
     * @throws ExeçãoVérticeNãoExistente caso esta estrutura não represente um
     *             grafo, isto é, não contenha vértices.
     */
    public boolean éCompleto() throws ExeçãoVérticeNãoExistente
    {
        final int n = this.ordem() - 1;
        
        final Set< ? > vérticesConjuto = this.vértices();
        final Iterator< ? > vértices = vérticesConjuto.iterator();
        
        while( vértices.hasNext() )
        {
            final Object próximo = vértices.next();
            
            if( ( ( this.grau( próximo ) != n ) && !this.temLaço( próximo ) )
                || ( ( this.grau( próximo ) != ( n + 1 ) ) && this
                    .temLaço( próximo ) ) )
            {
                return false;
            }
        }
        return true;
    }
    
    /**
     * Verifica se todos os vértices de G possuem o mesmo grau, ou seja, se este
     * é um grafo regular.
     * 
     * @return true se este é um grafo regular, false caso contrário.
     * @throws ExeçãoVérticeNãoExistente caso esta estrutura não represente um
     *             grafo, isto é, não contenha vértices.
     */
    public boolean éRegular() throws ExeçãoVérticeNãoExistente
    {
        final int grau = this.grau( this.umVértice() );
        
        final Set< ? > vérticesConjunto = this.vértices();
        final Iterator< ? > vértices = vérticesConjunto.iterator();
        
        while( vértices.hasNext() )
        {
            final int grauTemp = this.grau( vértices.next() );
            if( grau != grauTemp )
            {
                return false;
            }
        }
        return true;
    }
    
    /**
     * Verifica se dois vértices estão conectados.
     * 
     * @param vértice1 o primeiro vértice.
     * @param vértice2 o segundo vértice.
     * @return true se conectados, false caso contrário.
     * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
     */
    public boolean estãoConectados( final Object vértice1, final Object vértice2 )
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
     * Retorna um conjunto contendo todos os vértices deste Grafo que são
     * transitivamente alcancáveis partindo-se do vértice.
     * 
     * @param vértice um vértice deste grafo.
     * @return um conjunto contendo o fecho transitivo.
     */
    public Set< ? > fechoTransitivo( final Object vértice )
    {
        // TODO Auto-generated method stub
        
        return new HashSet<>();
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
     * Retorna o número de vértices deste Grafo..
     * 
     * @return um inteiro.
     */
    public int ordem()
    {
        return this.vértices.size();
    }
    
    /**
     * Remove um vértice deste Grafo., juntamente com todas as conexões.
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
     * Informa se um dado vértice deste grafo possui laço.
     * 
     * @param vértice um vértice deste grafo.
     * @return true caso possua laço, false contrário.
     * @throws ExeçãoVérticeNãoExistente não exista o vértice neste grafo.
     */
    public boolean temLaço( final Object vértice )
        throws ExeçãoVérticeNãoExistente
    {
        return this.estãoConectados( vértice, vértice );
    }
    
    /**
     * Retorna uma representação em string do grafo.
     */
    @Override
    public String toString()
    {
        String grafo = "( ";
        
        final Set< ? > vérticesConjuto = this.vértices();
        final Iterator< ? > vértices = vérticesConjuto.iterator();
        
        while( vértices.hasNext() )
        {
            final Object vértice = vértices.next();
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
            
            if( vértices.hasNext() )
            {
                grafo += ", ";
            }
        }
        grafo += " )";
        return grafo;
    }
    
    /**
     * Retorna um vértice qualquer deste Grafo..
     * 
     * @return um vértice.
     */
    public Object umVértice()
    {
        return this.vértices.keys().nextElement();
    }
    
    /**
     * Retorna um vértice qualquer deste Grafo..
     * 
     * @param vértice um vértice para retornar.
     * @return um vértice.
     * @throws ExeçãoVérticeNãoExistente caso o vértice não exista.
     */
    public Object umVértice( final Object vértice )
        throws ExeçãoVérticeNãoExistente
    {
        if( !this.contémVertice( vértice ) )
        {
            throw new ExeçãoVérticeNãoExistente( vértice, this );
        }
        return vértice;
    }
    
    /**
     * Retorna um conjunto contendo os vértices deste Grafo..
     * 
     * @return uma enumeração.
     */
    public Set< ? > vértices()
    {
        return this.vértices.keySet();
    }
}
