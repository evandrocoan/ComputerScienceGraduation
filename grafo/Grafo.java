package grafo;

import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
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
   /**
    * Resposável por realizar o debug do programa, quando ativado. Deve ser
    * instânciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instânciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( Grafo.class.getName() );
   
   /**
    * Serve para armazenar os vértices do grafo e suas arestas.
    */
   private final HashMap< Object, HashMap< Object, Object >> vértices;
   
   /**
    * Prepara a estrutura para ser utilizada como um grafo. Antes de ser criado
    * um grafo, este construtor precisa ser chamado.
    */
   public Grafo()
   {
      Grafo.LOG.setLevel( Level.OFF );
      this.vértices = new HashMap<>();
   }
   
   /**
    * Cria um grafo contendo o vértice passados como parâmetro.
    * 
    * @param vértice um vértices.
    */
   public Grafo( final Object vértice )
   {
      this(); // chama o construtor que prepara a estrutura
      try
      {
         this.adicionaVértice( vértice );
         
      } catch( final ExeçãoVérticeJáExistente e )
      {
         e.printStackTrace();
      }
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
      this.vértices.put( vértice, new HashMap<>() );
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
            final Enumeration< ? > adjacentes )
            throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
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
   public Set< Object > adjacentes( final Object vértice )
            throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      final HashMap< ?, ? > arestas = this.vértices.get( vértice );
      final Collection< ? > temporário = arestas.values();
      
      return new HashSet<>( Arrays.asList( temporário.toArray() ) );
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
      final Collection< ? > coleção = this.adjacentes( vértice );
      final Iterator< ? > iterador = coleção.iterator();
      final Object[] adjacentes = new Object[this.grau( vértice )];
      
      int índice = 0;
      while( iterador.hasNext() )
      {
         adjacentes[índice] = iterador.next();
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
      
      // pega a HashMap de arestas do vértice
      final HashMap< Object, Object > arestasDoVértice1 =
               this.vértices.get( vértice1 );
      final HashMap< Object, Object > arestasDoVértice2 =
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
      
      // pega a HashMap de arestas do vértice
      final HashMap< Object, Object > arestasDoVértice1 =
               this.vértices.get( vértice1 );
      final HashMap< Object, Object > arestasDoVértice2 =
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
   public boolean é�?rvore()
   {
      return this.éConexo() && !this.háCiclos();
   }
   
   /**
    * Verifica se cada vértice de G está conectados a todos os outros vértices.
    * 
    * @return true caso este grafo seja completo, false caso contrário.
    * @throws ExeçãoVérticeNãoExistente caso esta estrutura não represente um
    *            grafo, isto é, não contenha vértices.
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
    * Verifica se existe pelo menos um caminho que entre cada par de vértices
    * deste grafo.
    * 
    * @return true se este grafo é conexo, false caso contrário.
    */
   public boolean éConexo()
   {
      return this.vértices().equals( this.fechoTransitivo( this.umVértice() ) );
   }
   
   /**
    * Verifica se todos os vértices de G possuem o mesmo grau, ou seja, se este
    * é um grafo regular.
    * 
    * @return true se este é um grafo regular, false caso contrário.
    * @throws ExeçãoVérticeNãoExistente caso esta estrutura não represente um
    *            grafo, isto é, não contenha vértices.
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
      final HashMap< ?, ? > adjacentes = this.vértices.get( vértice1 );
      
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
   public Set< Object > fechoTransitivo( final Object vértice )
   {
      final Set< Object > jáVisitados = new HashSet<>();
      final Set< Object > fechoTransitivo = new HashSet<>();
      fechoTransitivo.add( vértice );
      return this
               .procuraFechoTransitivo( vértice, fechoTransitivo, jáVisitados );
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
      final HashMap< ?, ? > arestas = this.vértices.get( vértice );
      
      int size = 0;
      
      size = size + arestas.size();
      
      return size;
   }
   
   /**
    * Verifica se este grafo contém ciclos.
    * 
    * @return true se este grafo contém ciclos.
    */
   public boolean háCiclos()
   {
      final Set< Object > vérticesJáVisitados = new HashSet<>();
      
      final Set< Object > vérticesDoGrafo = this.vértices();
      final Iterator< Object > iterador = vérticesDoGrafo.iterator();
      
      // Chama a função recursiva que auxilia a detecção de ciclos em diferentes
      // árvores de busca profunda.
      while( iterador.hasNext() )
      {
         final Object vérticeAtual = iterador.next();
         
         if( !vérticesJáVisitados.contains( vérticeAtual ) )
         {
            if( this.háCiclosRecursivo( vérticeAtual, vérticeAtual,
                     vérticesJáVisitados, vérticesDoGrafo ) )
            {
               return true;
            }
         }
      }
      
      return false;
   }
   
   /**
    * Uma função recursiva que detecta ciclos em um subgrafo alcançável a partir
    * de um certo vértice.
    * 
    * @param vérticeAtual o vértice atual da busca.
    * @param vérticeAnterior o vértice anterior da busca.
    * @param vérticesJáVisitados um conjunto contendo os vértices já visitados.
    * @param vérticesDoGrafo um conjunto contendo todos os vértices deste grafo.
    * @return true se foi encontro um ciclo, false caso contrário.
    */
   private boolean háCiclosRecursivo( final Object vérticeAtual,
            final Object vérticeAnterior,
            final Set< Object > vérticesJáVisitados,
            final Set< Object > vérticesDoGrafo )
   {
      // Marca o vértice atual como já visitado
      vérticesJáVisitados.add( vérticeAtual );
      
      // Recorre para todos os vértices adjacentes a esse vértice.
      Set< Object > adjacentes = new HashSet<>();
      try
      {
         adjacentes = this.adjacentes( vérticeAtual );
         
      } catch( final ExeçãoVérticeNãoExistente e )
      {
         e.printStackTrace();
      }
      final Iterator< Object > iterador = adjacentes.iterator();
      
      while( iterador.hasNext() )
      {
         final Object próximoVértice = iterador.next();
         
         // Se um adjacente não é visitado, em seguida, recorre para estes
         // adjacentes
         if( !vérticesJáVisitados.contains( próximoVértice ) )
         {
            if( this.háCiclosRecursivo( próximoVértice, vérticeAtual,
                     vérticesJáVisitados, vérticesDoGrafo ) )
            {
               return true;
            }
         } else
            // Se um adjacente é visitado e não é o vértice anterior ao vértice
            // atual, então está ocorrendo um ciclo.
            if( !próximoVértice.equals( vérticeAnterior ) )
            {
               return true;
            }
      }
      return false;
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
   
   private Set< Object > procuraFechoTransitivo( final Object vértice,
            final Set< Object > fechoTransitivo, final Set< Object > jáVisitados )
   {
      jáVisitados.add( vértice );
      
      Collection< ? > adjacentes = new HashSet<>();
      try
      {
         adjacentes = this.adjacentes( vértice );
         
      } catch( final ExeçãoVérticeNãoExistente e )
      {
         e.printStackTrace();
      }
      final Iterator< ? > iterador = adjacentes.iterator();
      
      while( iterador.hasNext() )
      {
         final Object objeto = iterador.next();
         if( !jáVisitados.contains( objeto ) )
         {
            if( GrafoTest.LOG.isLoggable( Level.FINE ) )
            {
               System.out.println( "Objeto: " + objeto );
            }
            fechoTransitivo.add( objeto );
            this.procuraFechoTransitivo( objeto, fechoTransitivo, jáVisitados );
         }
      }
      return fechoTransitivo;
   }
   
   /**
    * Remove os laços de todos os vértices desse grafo.
    */
   public void removerLaços()
   {
      final Set< ? > vértices = this.vértices();
      final Iterator< ? > iterador = vértices.iterator();
      
      while( iterador.hasNext() )
      {
         final Object vértice = iterador.next();
         try
         {
            this.desconecta( vértice, vértice );
            
         } catch( final ExeçãoVérticeNãoExistente e )
         {
            e.printStackTrace();
         }
      }
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
      final Collection< ? > adjacentes = this.adjacentes( vértice );
      final Iterator< ? > iterador = adjacentes.iterator();
      
      while( iterador.hasNext() )
      {
         this.desconecta( vértice, iterador.next() );
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
         
         Collection< ? > adjacentes;
         Iterator< ? > iterador;
         try
         {
            adjacentes = this.adjacentes( vértice );
            iterador = adjacentes.iterator();
            
            while( iterador.hasNext() )
            {
               grafo = grafo + " " + iterador.next();
               
               if( iterador.hasNext() )
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
      return this.vértices.keySet().iterator().next();
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
   public Set< Object > vértices()
   {
      return this.vértices.keySet();
   }
}
