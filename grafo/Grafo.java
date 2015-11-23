package grafo;

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
 * Esta implementação pode ser feita em qualquer linguagem de programação, mas deve explicitamente
 * apresentar a estrutura de grafos. Por exemplo, se for feita numa linguagem de programação
 * orientada a objetos deve existir uma classe Grafo.
 *
 * Os critérios de avaliação incluem:
 *
 * estrutura do código; legibilidade do código; eficácia e eficiência (esperada complexidade O(1)
 * para as operações básicas)
 *
 * @author Evandro  Coan
 */
public class Grafo
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser Instanciado antes que o
    * construtor desta classe, pois este construtor precisa de deste objeto já instanciado para ser
    * monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( Grafo.class.getName() );
   
   /**
    * Serve para armazenar os vértices sucessores do grafo e arestas caso o grafo seja orientado. E
    * armazena os vértices do grafo e suas arestas caso o grafo seja não orientado.
    */
   private final HashMap< Object, HashSet< Object > > vértices;
   
   /**
    * Serve para armazenar os vértices antecessores dos vértices do grafo e suas arestas.
    */
   private final HashMap< Object, HashSet< Object > > vértices_Antecessores;
   
   /**
    * Serve para armazenar os pesos das arestas do grafo.
    */
   private final HashMap< String, Object > pesosDaAresta;
   
   /**
    * Serve para armazenar os pesos dos vértices do grafo.
    */
   private final HashMap< Object, Object > pesosDoVértice;
   
   /**
    * Controla se este será um grafo orientado ou não.
    */
   private boolean éOrientado;
   
   /**
    * Prepara a estrutura para ser utilizada como um grafo não orientado. Antes de ser criado um
    * grafo, este construtor precisa ser chamado.
    */
   public Grafo()
   {
      Grafo.LOG.setLevel( Level.OFF );
      this.vértices = new HashMap< >();
      this.vértices_Antecessores = new HashMap< >();
      this.pesosDaAresta = new HashMap< >();
      this.pesosDoVértice = new HashMap< >();
      this.éOrientado = false;
   }
   
   /**
    * Prepara a estrutura para ser utilizada como um grafo orientado. Antes de ser criado um grafo,
    * este construtor precisa ser chamado.
    * 
    * @param éOrientado true caso esse seja um grafo orientado.
    */
   public Grafo( final boolean éOrientado )
   {
      this();
      this.éOrientado = éOrientado;
   }
   
   /**
    * Cria um grafo contendo o vértice passados como parâmetro.
    *
    * @param vértice um vértice.
    * @param éOrientado true caso esse seja um grafo orientado.
    */
   public Grafo( final Object vértice, final boolean éOrientado )
   {
      this(); // chama o construtor que prepara a estrutura
      this.éOrientado = éOrientado;
      
      try
      {
         this.adicionaVértice( vértice );
         
      }
      catch( final ExeçãoVérticeJáExistente e )
      {
         e.printStackTrace();
      }
   }
   
   /**
    * Adiciona um peso ao vértice deste Grafo.
    *
    * @param vértice um vértice.
    * @param peso o peso a adicionar.
    *           
    * @throws ExeçãoVérticeNãoExistente caso o vértice não já exista.
    */
   public void adicionarPesoAoVértice( final Object vértice, final Object peso )
      throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      this.pesosDoVértice.put( vértice, peso );
   }
   
   /**
    * Adiciona peso um aresta conectando dois vértices.
    * 
    * @param vértice1 o primeiro vértice.
    * @param vértice2 o segundo vértice.
    * @param peso da aresta.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    */
   public void adicionarPesoAresta( final Object vértice1, final Object vértice2,
      final Object peso ) throws ExeçãoVérticeNãoExistente
   {
      this.conecta( vértice1, vértice2, peso );
   }
   
   /**
    * Adiciona um novo vértice neste Grafo.
    *
    * @param vértice um vértice.
    *           
    * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
    */
   public void adicionaVértice( final Object vértice ) throws ExeçãoVérticeJáExistente
   {
      if( this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeJáExistente( vértice, this );
      }
      this.vértices.put( vértice, new HashSet< >() );
      this.vértices_Antecessores.put( vértice, new HashSet< >() );
   }
   
   /**
    * Adiciona um novo vértice neste Grafo, conectado a vários vértices.
    *
    * @param vértice um vértice.
    * @param adjacentes uma enumeração de vértices adjacentes.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
    */
   public void adicionaVérticeConectado( final Object vértice, final Enumeration< ? > adjacentes )
      throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      this.adicionaVértice( vértice );
      
      while( adjacentes.hasMoreElements() )
      {
         this.conecta( vértice, adjacentes.nextElement() );
      }
   }
   
   /**
    * Adiciona um novo vértice neste Grafo, conectado a vários vértices.
    *
    * @param vértice um vértice.
    * @param adjacentes um arranjo de vértices adjacentes.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
    */
   public void adicionaVérticeConectado( final Object vértice, final Object[] adjacentes )
      throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      this.adicionaVértice( vértice );
      
      for( int índice = 0; índice < adjacentes.length; índice++ )
      {
         this.conecta( vértice, adjacentes[índice] );
      }
   }
   
   /**
    * Adiciona um novo vértice neste Grafo, conectado a vários vértices.
    *
    * @param vértice um vértice.
    * @param adjacentes um vetor de vértices adjacentes.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    * @throws ExeçãoVérticeJáExistente caso o vértice já exista.
    */
   public void adicionaVérticeConectado( final Object vértice, final Vector< ? > adjacentes )
      throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      this.adicionaVértice( vértice );
      
      final Iterator< ? > adjacentesIterador = adjacentes.iterator();
      
      while( adjacentesIterador.hasNext() )
      {
         this.conecta( vértice, adjacentesIterador.next() );
      }
   }
   
   /**
    * Adiciona vários novos vértices neste Grafo.
    *
    * @param vértices uma enumeração de vértices para se adicionar.
    *           
    * @throws ExeçãoVérticeJáExistente caso algum vértice já exista.
    */
   public void adicionaVértices( final Enumeration< ? > vértices ) throws ExeçãoVérticeJáExistente
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
    * Adiciona vários novos vértices neste Grafo.
    *
    * @param vértices um arranjo de vértices.
    *           
    * @throws ExeçãoVérticeJáExistente caso algum vértice já exista.
    */
   public void adicionaVértices( final Object[] vértices ) throws ExeçãoVérticeJáExistente
   {
      for( int índice = 0; índice < vértices.length; índice++ )
      {
         this.adicionaVértice( vértices[índice] );
      }
   }
   
   /**
    * Adiciona vários novos vértices neste Grafo.
    *
    * @param vértices um vetor de vértices.
    *           
    * @throws ExeçãoVérticeJáExistente caso algum vértice já exista.
    */
   public void adicionaVértices( final Vector< ? > vértices ) throws ExeçãoVérticeJáExistente
   {
      final Iterator< ? > vérticesIterador = vértices.iterator();
      
      while( vérticesIterador.hasNext() )
      {
         this.adicionaVértice( vérticesIterador.next() );
      }
      
   }
   
   /**
    * Retorna os vértices adjacentes de um dado vértice caso esse grafo seja orientado. Caso este
    * grafo seja orientado, retorna os vértices sucessores do vértice atual.
    *
    * @param vértice um vértice pertencente a este Grafo.
    * @return os vértices como um conjunto da interface Set<>().
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
    */
   public Set< Object > adjacentes( final Object vértice ) throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      return this.vértices.get( vértice );
   }
   
   /**
    * Retorna os vértices antecessores de um dado vértice, caso este grafo seja orientado.
    *
    * @param vértice um vértice pertencente a este Grafo.
    * @return os vértices como um conjunto da interface Set<>().
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
    */
   public Set< Object > adjacentes_Antecessores( final Object vértice )
      throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      return this.vértices_Antecessores.get( vértice );
   }
   
   /**
    * Retorna os vértices sucessores de um dado vértice, caso este grafo seja orientado.
    *
    * @param vértice um vértice pertencente a este Grafo.
    * @return os vértices como um conjunto da interface Set<>().
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
    */
   public Set< Object > adjacentes_Sucessores( final Object vértice )
      throws ExeçãoVérticeNãoExistente
   {
      return this.adjacentes( vértice );
   }
   
   /**
    * Retorna os vértices adjacentes de um dado vértice.
    *
    * @param vértice um vértice pertencente a este Grafo.
    * @return os vértices como um array.
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
    */
   public Object[] adjacentesEmArranjo( final Object vértice ) throws ExeçãoVérticeNãoExistente
   {
      final Collection< Object > coleção = this.adjacentes( vértice );
      final Iterator< Object > iterador = coleção.iterator();
      final Object[] adjacentes = new Object[this.grau( vértice )];
      
      int índice = 0;
      while( iterador.hasNext() )
      {
         adjacentes[índice] = iterador.next();
         índice++;
      }
      return adjacentes;
   }
   
   public void ajustarPesosDetodosOsVértices( final Object pesoNovo )
   {
      final Iterator< ? > vertices = this.vértices().iterator();
      
      while( vertices.hasNext() )
      {
         try
         {
            this.adicionarPesoAoVértice( vertices.next(), pesoNovo );
         }
         catch( final ExeçãoVérticeNãoExistente exeption )
         {
            exeption.printStackTrace();
         }
      }
   }
   
   /**
    * Realiza os cálculos dos tempos de atraso máximo da atividades.
    * 
    * @param temposMaisCedo o grafo contendo os tempos mais cedo.
    * @param temposMaisTarde o grafo contendo os tempos mais tarde.
    *           
    * @return um grafo contendo os tempos de atraso máximo.
    */
   public Grafo calcularOsTemposDeAtrazoMáximo( final Grafo temposMaisCedo,
      final Grafo temposMaisTarde )
   {
      final Iterator< ? > vértices = temposMaisCedo.vértices().iterator();
      
      while( vértices.hasNext() )
      {
         final Object próximo = vértices.next();
         Integer tempoMaisCedo_inteiro = new Integer( 0 );
         Integer tempoMaisTarde_inteiro = new Integer( 0 );
         
         try
         {
            tempoMaisCedo_inteiro = (Integer) temposMaisTarde.pesoDoVértice( próximo );
            tempoMaisTarde_inteiro = (Integer) temposMaisCedo.pesoDoVértice( próximo );
         }
         catch( final ExeçãoVérticeNãoExistente exeption )
         {
            exeption.printStackTrace();
         }
         final Integer atrasoMáximo = new Integer(
            tempoMaisCedo_inteiro.intValue() - tempoMaisTarde_inteiro.intValue() );
            
         try
         {
            this.adicionarPesoAoVértice( próximo, atrasoMáximo );
         }
         catch( final ExeçãoVérticeNãoExistente exeption )
         {
            exeption.printStackTrace();
         }
      }
      
      return this;
   }
   
   /**
    * Calcula os tempos mais cedo de cada vértice do grafo.
    * 
    * @param vérticeFonte o vértice fonte deste grafo.
    *           
    * @return um grafo valorado orientado contento os tempos mais cedo de cada vértice.
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vérticeInicial não exista.
    */
   public Grafo calcularOsTemposMaisCedo( final Object vérticeFonte )
      throws ExeçãoVérticeNãoExistente
   {
      /* if( this.éConexo() && !this.háCiclos() && !( this.adjacentes_Antecessores( vérticeInicial
       * ).isEmpty() ) ) { */
      this.ajustarPesosDetodosOsVértices( new Integer( 0 ) );
      this.calculoDoTempoMaisCedo( vérticeFonte, 0 );
      // }
      return this;
   }
   
   /**
    * Calcula os tempos mais tarde de cada vértice do grafo.
    * 
    * @param vérticeSumidouro o vértice sumidouro deste grafo.
    * @param vérticeFonte o vértice fonte deste grafo.
    *           
    * @return um grafo valorado orientado contento os tempos mais cedo de cada vértice.
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vérticeInicial não exista.
    */
   public Grafo calcularOsTemposMaisTarde( final Object vérticeFonte,
      final Object vérticeSumidouro ) throws ExeçãoVérticeNãoExistente
   {
      /* if( this.éConexo() && !this.háCiclos() && !( this.adjacentes_Antecessores( vérticeInicial
       * ).isEmpty() ) ) { */
      this.calcularOsTemposMaisCedo( vérticeFonte );
      
      final Integer maiorPeso = (Integer) this.pesoDoMaiorVértice();
      
      this.ajustarPesosDetodosOsVértices( maiorPeso );
      this.calculoDoTempoMaisTarde( vérticeSumidouro, maiorPeso.intValue() );
      // }
      return this;
   }
   
   /**
    * Realiza recursivamente a busca dos tempos mais cedo a partir de um vértice qualquer com um
    * peso inicial qualquer.
    * 
    * @param vérticeAtual o vértice que atualmente se está inspecionando.
    * @param vérticeInicial o vértice onde se iniciou a busca.
    * @param vérticesJáVisitados os vértices já visitados na antes da inspeção atual.
    * @throws ExeçãoVérticeNãoExistente caso o vérticeAtual não exista.
    */
   private void calculoDoTempoMaisCedo( final Object vérticeAtual, final int pesoInicial )
      throws ExeçãoVérticeNãoExistente
   {
      this.adicionarPesoAoVértice( vérticeAtual, new Integer( pesoInicial ) );
      
      final Iterator< ? > sucessores = this.adjacentes_Sucessores( vérticeAtual ).iterator();
      
      while( sucessores.hasNext() )
      {
         final Object sucessor = sucessores.next();
         final Object pesoDoVérticeSucessor_Integer = this.pesoDoVértice( sucessor );
         
         final int pesoDoVérticeSucessor = ( (Integer) pesoDoVérticeSucessor_Integer ).intValue();
         final int pesoDaArestaDeOrigem = ( (Integer) this.pesoDaAresta( vérticeAtual, sucessor ) )
            .intValue();
            
         final int pesoNovoDoVérticeSucessor = pesoDaArestaDeOrigem + pesoInicial;
         
         if( pesoDoVérticeSucessor < pesoNovoDoVérticeSucessor )
         {
            this.adicionarPesoAoVértice( sucessor, new Integer( pesoNovoDoVérticeSucessor ) );
            this.calculoDoTempoMaisCedo( sucessor, pesoNovoDoVérticeSucessor );
         }
         else
         {
            this.calculoDoTempoMaisCedo( sucessor, pesoDoVérticeSucessor );
         }
      }
   }
   
   /**
    * Realiza recursivamente a busca dos tempos mais cedo a partir de um vértice qualquer com um
    * peso inicial qualquer.
    * 
    * @param vérticeAtual o vértice que atualmente se está inspecionando.
    * @param vérticeInicial o vértice onde se iniciou a busca.
    * @param vérticesJáVisitados os vértices já visitados na antes da inspeção atual.
    *           
    * @throws ExeçãoVérticeNãoExistente caso o vérticeAtual não exista.
    */
   private void calculoDoTempoMaisTarde( final Object vérticeAtual, final int pesoInicial )
      throws ExeçãoVérticeNãoExistente
   {
      this.adicionarPesoAoVértice( vérticeAtual, new Integer( pesoInicial ) );
      
      final Iterator< ? > antecessores = this.adjacentes_Antecessores( vérticeAtual ).iterator();
      
      while( antecessores.hasNext() )
      {
         final Object antecessor = antecessores.next();
         final Object pesoDoVérticeAntecessor_Integer = this.pesoDoVértice( antecessor );
         
         final int pesoDoVérticeAntecessor = ( (Integer) pesoDoVérticeAntecessor_Integer )
            .intValue();
         final int pesoDaArestaOrigem = ( (Integer) this.pesoDaAresta( antecessor, vérticeAtual ) )
            .intValue();
            
         final int pesoNovoDoVérticeAntecessor = pesoInicial - pesoDaArestaOrigem;
         
         if( pesoDoVérticeAntecessor > pesoNovoDoVérticeAntecessor )
         {
            this.adicionarPesoAoVértice( antecessor, new Integer( pesoNovoDoVérticeAntecessor ) );
            this.calculoDoTempoMaisTarde( antecessor, pesoNovoDoVérticeAntecessor );
         }
         else
         {
            this.calculoDoTempoMaisTarde( antecessor, pesoDoVérticeAntecessor );
         }
         
      }
   }
   
   /**
    * Conecta um vértice1 em outros vértices pertencentes a este Grafo.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértices uma enumeração de vértices.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
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
    * Conecta dois vértices pertencentes a este Grafo.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértice2 o segundo vértice.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
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
      // pega a HashSet de arestas do vértice
      final HashSet< Object > arestasDoVértice1 = this.vértices.get( vértice1 );
      
      // conecta o vértice1 com o vértice2
      arestasDoVértice1.add( vértice2 );
      
      if( this.éOrientado )
      {
         final HashSet< Object > vértices_Antecessores = this.vértices_Antecessores.get( vértice2 );
         vértices_Antecessores.add( vértice1 );
      }
      else
      {
         final HashSet< Object > arestasDoVértice2 = this.vértices.get( vértice2 );
         arestasDoVértice2.add( vértice1 );
      }
   }
   
   /**
    * Conecta dois vértices pertencentes a este Grafo.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértice2 o segundo vértice.
    * @param peso da aresta.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    */
   public void conecta( final Object vértice1, final Object vértice2, final Object peso )
      throws ExeçãoVérticeNãoExistente
   {
      final String aresta = new Integer( vértice1.hashCode() ).toString() + " conecta "
         + new Integer( vértice2.hashCode() ).toString();
         
      this.conecta( vértice1, vértice2 );
      this.pesosDaAresta.put( aresta, peso );
   }
   
   /**
    * Conecta um vértice1 em outros vértices pertencentes a este Grafo.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértices um array de vértices.
    *           
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
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
    * Conecta todos os vértices1 em todos os vértices2.
    *
    * @param vértices1 o primeiro array de vértices.
    * @param vértices2 o segundo array de vértices.
    *           
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
    * Informa se um vértice existe nesse grafo.
    *
    * @param vértice um vértice.
    *           
    * @return true caso exista, false caso contrário.
    */
   public boolean contémVertice( final Object vértice )
   {
      return this.vértices.containsKey( vértice );
   }
   
   /**
    * Desconecta dois vértices pertencentes a este Grafo.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértice2 o segundo vértice.
    *           
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
      // pega a HashMap de arestas do vértice
      final HashSet< Object > arestasDoVértice1 = this.vértices.get( vértice1 );
      final HashSet< Object > arestasDoVértice2 = this.vértices.get( vértice2 );
      
      // desconecta o vértice1 do vértice2
      arestasDoVértice1.remove( vértice2 );
      arestasDoVértice2.remove( vértice1 );
   }
   
   /**
    * Verifica se este Grafo é uma árvore, ou seja, verifica se não há ciclos e se este é um grafo
    * conexo.
    *
    * @return true caso seja uma arvore, false caso contrário.
    */
   public boolean éÁrvore()
   {
      return this.éConexo() && !this.háCiclos();
   }
   
   /**
    * Verifica se este Grafo é completo, ou seja, caso todos os vértices deste Grafo estão
    * conectados a todos os outros vértices.
    *
    * @return true caso este grafo seja completo, false caso contrário.
    *         
    * @throws ExeçãoVérticeNãoExistente caso esta estrutura não represente um grafo, isto é, não
    *            contenha vértices.
    */
   public boolean éCompleto() throws ExeçãoVérticeNãoExistente
   {
      final int n = this.ordem() - 1;
      
      final Set< Object > vérticesConjuto = this.vértices();
      final Iterator< Object > vértices = vérticesConjuto.iterator();
      
      while( vértices.hasNext() )
      {
         final Object próximo = vértices.next();
         
         if( ( ( this.grau( próximo ) != n ) && !this.temLaço( próximo ) )
            || ( ( this.grau( próximo ) != ( n + 1 ) ) && this.temLaço( próximo ) ) )
         {
            return false;
         }
      }
      return true;
   }
   
   /**
    * Verifica se este Grafo é conexo, ou seja, caso existe pelo menos um caminho que entre cada par
    * de vértices deste grafo.
    *
    * @return true caso este grafo seja conexo, false caso contrário.
    */
   public boolean éConexo()
   {
      return this.vértices().equals( this.fechoTransitivo( this.umVértice() ) );
   }
   
   /**
    * Verifica se este grafo é regular, ou seja, caso todos os vértices deste Grafo possuam o mesmo
    * grau.
    *
    * @return true caso este seja um grafo regular, false caso contrário.
    *         
    * @throws ExeçãoVérticeNãoExistente caso esta estrutura não represente um grafo, isto é, não
    *            contenha vértices.
    */
   public boolean éRegular() throws ExeçãoVérticeNãoExistente
   {
      final int grau = this.grau( this.umVértice() );
      
      final Set< Object > vérticesConjunto = this.vértices();
      final Iterator< Object > vértices = vérticesConjunto.iterator();
      
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
    * Verifica se dois vértices deste Grafo estão conectados.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértice2 o segundo vértice.
    *           
    * @return true caso estejam conectados, false caso contrário.
    *         
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
      final HashSet< Object > adjacentes = this.vértices.get( vértice1 );
      
      return adjacentes.contains( vértice2 );
   }
   
   /**
    * Retorna o fecho transitivo a partir de um vértice deste Grafo. Ou seja, um conjunto contendo
    * todos os vértices deste Grafo que são transitivamente alcançáveis partindo-se de um vértice.
    *
    * @param vértice um vértice deste grafo.
    *           
    * @return um conjunto da interface Set<>().
    */
   public Set< Object > fechoTransitivo( final Object vértice )
   {
      final Set< Object > jáVisitados = new HashSet< >();
      final Set< Object > fechoTransitivo = new HashSet< >();
      fechoTransitivo.add( vértice );
      return this.fechoTransitivoBusca( vértice, fechoTransitivo, jáVisitados );
   }
   
   private Set< Object > fechoTransitivoBusca( final Object vértice,
      final Set< Object > fechoTransitivo, final Set< Object > jáVisitados )
   {
      jáVisitados.add( vértice );
      
      Collection< Object > adjacentes = new HashSet< >();
      try
      {
         adjacentes = this.adjacentes( vértice );
         
      }
      catch( final ExeçãoVérticeNãoExistente e )
      {
         e.printStackTrace();
      }
      final Iterator< Object > iterador = adjacentes.iterator();
      
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
            this.fechoTransitivoBusca( objeto, fechoTransitivo, jáVisitados );
         }
      }
      return fechoTransitivo;
   }
   
   /**
    * Retorna o número de vértices adjacentes a um vértice neste Grafo.
    *
    * @param vértice um vértice.
    *           
    * @return um inteiro.
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
    */
   public int grau( final Object vértice ) throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      final HashSet< Object > arestas = this.vértices.get( vértice );
      
      int size = 0;
      
      size = size + arestas.size();
      
      return size;
   }
   
   /**
    * Verifica se este grafo contém ciclos.
    *
    * @return true caso contenha ciclos, false caso contrário.
    */
   public boolean háCiclos()
   {
      final Set< Object > vérticesJáVisitados = new HashSet< >();
      
      final Set< Object > vérticesDoGrafo = this.vértices();
      final Iterator< Object > iterador = vérticesDoGrafo.iterator();
      
      // Chama a função recursiva que auxilia a detecção de ciclos em diferentes
      // árvores de busca profunda.
      while( iterador.hasNext() )
      {
         final Object vérticeAtual = iterador.next();
         
         if( !vérticesJáVisitados.contains( vérticeAtual ) )
         {
            if( this.háCiclosBusca( vérticeAtual, vérticeAtual, vérticesJáVisitados,
               vérticesDoGrafo ) )
            {
               return true;
            }
         }
      }
      return false;
   }
   
   private boolean háCiclosBusca( final Object vérticeAtual, final Object vérticeAnterior,
      final Set< Object > vérticesJáVisitados, final Set< Object > vérticesDoGrafo )
   {
      // Marca o vértice atual como já visitado
      vérticesJáVisitados.add( vérticeAtual );
      
      // Recorre para todos os vértices adjacentes a esse vértice.
      Set< Object > adjacentes = new HashSet< >();
      try
      {
         adjacentes = this.adjacentes( vérticeAtual );
         
      }
      catch( final ExeçãoVérticeNãoExistente e )
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
            if( this.háCiclosBusca( próximoVértice, vérticeAtual, vérticesJáVisitados,
               vérticesDoGrafo ) )
            {
               return true;
            }
         }
         else
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
    * Retorna o número de vértices deste Grafo.
    *
    * @return um inteiro.
    */
   public int ordem()
   {
      return this.vértices.size();
   }
   
   /**
    * Retorna o peso da aresta entre dois vértices.
    *
    * @param vértice1 o primeiro vértice.
    * @param vértice2 o segundo vértice.
    *           
    * @return o peso da aresta
    *         
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    */
   public Object pesoDaAresta( final Object vértice1, final Object vértice2 )
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
      
      final String aresta = new Integer( vértice1.hashCode() ).toString() + " conecta "
         + new Integer( vértice2.hashCode() ).toString();
         
      return this.pesosDaAresta.get( aresta );
   }
   
   public Object pesoDoMaiorVértice()
   {
      final Iterator< ? > vértices = this.vértices().iterator();
      
      Integer maior = new Integer( 0 );
      
      while( vértices.hasNext() )
      {
         Integer próximo = new Integer( 0 );
         try
         {
            próximo = (Integer) this.pesoDoVértice( vértices.next() );
         }
         catch( final ExeçãoVérticeNãoExistente exeption )
         {
            exeption.printStackTrace();
         }
         
         if( próximo.intValue() > maior.intValue() )
         {
            maior = próximo;
         }
      }
      return maior;
   }
   
   /**
    * Retorna o peso da aresta entre dois vértices.
    *
    * @param vértice1 o primeiro vértice.
    *           
    * @return o peso do vértice, null caso não tenha peso.
    *         
    * @throws ExeçãoVérticeNãoExistente caso algum vértice não seja encontrado.
    */
   public Object pesoDoVértice( final Object vértice1 ) throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice1 ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice1, this );
      }
      return this.pesosDoVértice.get( vértice1 );
   }
   
   /**
    * Remove os laços de todos os vértices desse grafo.
    */
   public void removerLaços()
   {
      final Set< Object > vértices = this.vértices();
      final Iterator< Object > iterador = vértices.iterator();
      
      while( iterador.hasNext() )
      {
         final Object vértice = iterador.next();
         try
         {
            this.desconecta( vértice, vértice );
            
         }
         catch( final ExeçãoVérticeNãoExistente e )
         {
            e.printStackTrace();
         }
      }
   }
   
   /**
    * Remove um vértice deste Grafo, juntamente com todas suas as conexões.
    *
    * @param vértice um vértice deste Grafo.
    *           
    * @throws ExeçãoVérticeNãoExistente caso o vértice não seja encontrado.
    */
   public void removerVértice( final Object vértice ) throws ExeçãoVérticeNãoExistente
   {
      if( !this.vértices.containsKey( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      final Collection< Object > adjacentes = this.adjacentes( vértice );
      final Iterator< Object > iterador = adjacentes.iterator();
      
      while( iterador.hasNext() )
      {
         this.desconecta( vértice, iterador.next() );
      }
      this.vértices.remove( vértice );
   }
   
   /**
    * Informa quantos vértices este Grafo possui.
    *
    * @return um inteiro.
    */
   public int tamanhoDoGrafo()
   {
      return this.vértices.size();
   }
   
   /**
    * Informa se um certo vértice deste grafo possui laço.
    *
    * @param vértice um vértice.
    *           
    * @return true caso o vértice possua laço, false contrário.
    *         
    * @throws ExeçãoVérticeNãoExistente caso não exista o vértice neste grafo.
    */
   public boolean temLaço( final Object vértice ) throws ExeçãoVérticeNãoExistente
   {
      return this.estãoConectados( vértice, vértice );
   }
   
   /**
    * Retorna uma representação em string deste grafo.
    */
   @Override
   public String toString()
   {
      String grafo = "( ";
      
      final Set< Object > vérticesConjuto = this.vértices();
      final Iterator< Object > vértices = vérticesConjuto.iterator();
      
      while( vértices.hasNext() )
      {
         final Object vértice = vértices.next();
         
         try
         {
            if( this.pesoDoVértice( vértice ) == null )
            {
               grafo += vértice + "[" + "0" + "]" + "(";
            }
            else
            {
               grafo += vértice + "[" + this.pesoDoVértice( vértice ) + "]" + "(";
            }
         }
         catch( final ExeçãoVérticeNãoExistente exeption )
         {
            exeption.printStackTrace();
         }
         
         Collection< Object > adjacentes;
         Iterator< Object > iterador;
         try
         {
            adjacentes = this.adjacentes( vértice );
            iterador = adjacentes.iterator();
            
            while( iterador.hasNext() )
            {
               final Object next = iterador.next();
               
               if( this.pesoDaAresta( vértice, next ) == null )
               {
                  grafo = grafo + " " + next + "[" + "0" + "]";
               }
               else
               {
                  grafo = grafo + " " + next + "[" + this.pesoDaAresta( vértice, next ) + "]";
               }
               
               if( iterador.hasNext() )
               {
                  grafo += ",";
               }
               else
               {
                  grafo += " ";
               }
            }
         }
         catch( final ExeçãoVérticeNãoExistente exeção )
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
    * Retorna um vértice qualquer deste Grafo.
    *
    * @return um vértice.
    */
   public Object umVértice()
   {
      return this.vértices.keySet().iterator().next();
   }
   
   /**
    * Retorna um vértice qualquer deste Grafo.
    *
    * @param vértice um vértice.
    *           
    * @return o vértice.
    *         
    * @throws ExeçãoVérticeNãoExistente caso o vértice não exista.
    */
   public Object umVértice( final Object vértice ) throws ExeçãoVérticeNãoExistente
   {
      if( !this.contémVertice( vértice ) )
      {
         throw new ExeçãoVérticeNãoExistente( vértice, this );
      }
      return vértice;
   }
   
   /**
    * Retorna um conjunto contendo todos os vértices deste Grafo.
    *
    * @return um conjunto da interface Set<>().
    */
   public Set< Object > vértices()
   {
      return this.vértices.keySet();
   }
}
