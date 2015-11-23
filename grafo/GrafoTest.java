package grafo;

import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runners.MethodSorters;

@FixMethodOrder( MethodSorters.NAME_ASCENDING )
public class GrafoTest
{
   /**
    * Responsável por realizar o debug do programa.
    */
   static final Logger LOG = Logger.getLogger( GrafoTest.class.getName() );
   
   private static Grafo grafo;
   
   @Rule
   public TestWatcher watchman = new TestWatcher()
   {
      @Override
      protected void starting( final Description description )
      {
         final String methodName = description.getMethodName();
         String className = description.getClassName();
         className = className.substring( className.lastIndexOf( '.' ) + 1 );
         final String message = className + " " + methodName;
         System.out.println( "Starting JUnit-test: " + message + " " );
      }
   };
   
   @BeforeClass
   public static void configureLOG()
   {
      GrafoTest.LOG.setLevel( Level.INFO );
   }
   
   @After
   public void logResults()
   {
      // GrafoTest.LOG.info( GrafoTest.grafo.toString() + "\n " );
   }
   
   /**
    * É executado antes que um teste inicia. Os testes executaram na seguinte ordem: setUp(),
    * test1(), printBye(), setUp(), test2(), printBye()...
    */
   @Before
   public void setUp()
   {
      GrafoTest.grafo = new Grafo();
   }
   
   @Test
   public void testAdicionarVértice() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      GrafoTest.grafo.adicionaVértice( "Brasil" );
      Assert.assertEquals( 0, GrafoTest.grafo.grau( "Brasil" ) );
      
      GrafoTest.grafo.adicionaVértice( "China" );
      GrafoTest.grafo.conecta( "Brasil", "China" );
      Assert.assertEquals( 1, GrafoTest.grafo.grau( "Brasil" ) );
   }
   
   @Test
   public void testAdicionarVérticeConectadoÀObjectEnumeration()
      throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      final Vector< String > adjacentesVector = new Vector< >();
      adjacentesVector.add( nomes[1] );
      adjacentesVector.add( nomes[4] );
      final Enumeration< String > adjacentes = adjacentesVector.elements();
      
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.adicionaVérticeConectado( "França", adjacentes );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
      
      GrafoTest.grafo.adicionaVértice( "Tcheca" );
      GrafoTest.grafo.conecta( "Tcheca", "França" );
      Assert.assertEquals( 3, GrafoTest.grafo.grau( "França" ) );
   }
   
   @Test
   public void testAdicionarVérticeConectadoÀObjectObjectArray()
      throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes );
      Assert.assertEquals( 5, GrafoTest.grafo.grau( nomes[0] ) );
      
      GrafoTest.grafo.adicionaVértice( "Tcheca" );
      GrafoTest.grafo.conecta( "Tcheca", nomes[0] );
      Assert.assertEquals( 6, GrafoTest.grafo.grau( nomes[0] ) );
      
      GrafoTest.grafo.conecta( "Tcheca", "Tcheca" );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "Tcheca" ) );
   }
   
   @Test
   public void testAdjacentes() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final String[] nomesArray = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomesArray );
      GrafoTest.grafo.conecta( nomesArray[0], nomesArray );
      
      final Vector< String > nomesVetor = new Vector< >( Arrays.asList( nomesArray ) );
      final Iterator< String > nomesIterador = nomesVetor.iterator();
      
      final Collection< Object > adjacentes = GrafoTest.grafo.adjacentes( nomesArray[0] );
      
      while( nomesIterador.hasNext() )
      {
         final Object objeto = nomesIterador.next();
         Assert.assertTrue( adjacentes.contains( objeto ) );
      }
      Assert.assertTrue( 5 == GrafoTest.grafo.ordem() );
   }
   
   @Test
   public void testConectarArestasComPeso()
      throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final String vértice1 = "Vertice1";
      final String vértice2 = "Vertice2";
      final String vértice3 = "Vertice3";
      final String peso1 = "Peso1";
      final String peso2 = "Peso2";
      
      GrafoTest.grafo.adicionaVértice( vértice1 );
      GrafoTest.grafo.adicionaVértice( vértice2 );
      GrafoTest.grafo.adicionaVértice( vértice3 );
      
      Assert.assertTrue( GrafoTest.grafo.pesoDaAresta( vértice1, vértice2 ) == null );
      Assert.assertFalse( GrafoTest.grafo.estãoConectados( vértice1, vértice2 ) );
      
      GrafoTest.grafo.conecta( vértice1, vértice2, peso1 );
      
      Assert.assertTrue( GrafoTest.grafo.estãoConectados( vértice1, vértice2 ) );
      Assert.assertFalse( GrafoTest.grafo.estãoConectados( vértice1, vértice3 ) );
      
      Assert.assertTrue( GrafoTest.grafo.pesoDaAresta( vértice1, vértice2 ).equals( peso1 ) );
      Assert.assertFalse( GrafoTest.grafo.pesoDaAresta( vértice1, vértice2 ).equals( peso2 ) );
   }
   
   @Test
   public void testDesconectarVérticesObjectObject()
      throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      final Vector< String > adjacentes = new Vector< >();
      adjacentes.add( nomes[1] );
      adjacentes.add( nomes[4] );
      
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.adicionaVérticeConectado( "França", adjacentes );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
      
      GrafoTest.grafo.adicionaVértice( "Tcheca" );
      GrafoTest.grafo.conecta( "Tcheca", "França" );
      Assert.assertEquals( 3, GrafoTest.grafo.grau( "França" ) );
      
      GrafoTest.grafo.desconecta( "Tcheca", "França" );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
      Assert.assertEquals( 0, GrafoTest.grafo.grau( "Tcheca" ) );
   }
   
   @Test
   public void testÉÁrvore1() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      Assert.assertFalse( GrafoTest.grafo.éÁrvore() );
   }
   
   @Test
   public void testÉÁrvore2() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes[1] );
      GrafoTest.grafo.conecta( nomes[0], nomes[2] );
      GrafoTest.grafo.conecta( nomes[1], nomes[3] );
      GrafoTest.grafo.conecta( nomes[1], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éÁrvore() );
      
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.conecta( nomes[2], "João1" );
      GrafoTest.grafo.conecta( nomes[2], "João2" );
      Assert.assertTrue( GrafoTest.grafo.éÁrvore() );
   }
   
   @Test
   public void testÉÁrvore3() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes[1] );
      GrafoTest.grafo.conecta( nomes[0], nomes[2] );
      GrafoTest.grafo.conecta( nomes[1], nomes[3] );
      GrafoTest.grafo.conecta( nomes[1], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éÁrvore() );
      
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.conecta( nomes[0], "João1" );
      GrafoTest.grafo.conecta( nomes[0], "João2" );
      Assert.assertTrue( GrafoTest.grafo.éÁrvore() );
   }
   
   @Test
   public void testÉÁrvore4() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes[1] );
      GrafoTest.grafo.conecta( nomes[0], nomes[2] );
      GrafoTest.grafo.conecta( nomes[1], nomes[3] );
      GrafoTest.grafo.conecta( nomes[1], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éÁrvore() );
      
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.conecta( nomes[0], "João1" );
      GrafoTest.grafo.conecta( nomes[0], "João2" );
      GrafoTest.grafo.conecta( "João1", "João2" );
      Assert.assertFalse( GrafoTest.grafo.éÁrvore() );
   }
   
   @Test
   public void testÉCompleto() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      Assert.assertTrue( GrafoTest.grafo.éCompleto() );
      
      GrafoTest.grafo.desconecta( nomes[0], nomes[0] );
      GrafoTest.grafo.desconecta( nomes[1], nomes[1] );
      GrafoTest.grafo.desconecta( nomes[2], nomes[2] );
      GrafoTest.grafo.desconecta( nomes[3], nomes[3] );
      GrafoTest.grafo.desconecta( nomes[4], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éCompleto() );
      
      GrafoTest.grafo.desconecta( nomes[0], nomes[1] );
      Assert.assertFalse( GrafoTest.grafo.éCompleto() );
      
      GrafoTest.grafo.conecta( nomes[0], nomes[0] );
      GrafoTest.grafo.conecta( nomes[1], nomes[1] );
      GrafoTest.grafo.conecta( nomes[2], nomes[2] );
      GrafoTest.grafo.conecta( nomes[3], nomes[3] );
      GrafoTest.grafo.conecta( nomes[4], nomes[4] );
      Assert.assertFalse( GrafoTest.grafo.éCompleto() );
      
      GrafoTest.grafo.conecta( nomes[0], nomes[1] );
      GrafoTest.grafo.desconecta( nomes[1], nomes[4] );
      Assert.assertFalse( GrafoTest.grafo.éCompleto() );
      
      GrafoTest.grafo.conecta( nomes[1], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éCompleto() );
   }
   
   @Test
   public void testÉConexo() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      Assert.assertTrue( GrafoTest.grafo.éConexo() );
      
      GrafoTest.grafo.removerLaços();
      Assert.assertTrue( GrafoTest.grafo.éConexo() );
      
      GrafoTest.grafo.adicionaVértice( "João" );
      Assert.assertFalse( GrafoTest.grafo.éConexo() );
      
      GrafoTest.grafo.conecta( nomes[0], nomes[0] );
      GrafoTest.grafo.conecta( nomes[1], nomes[1] );
      GrafoTest.grafo.conecta( nomes[2], nomes[2] );
      GrafoTest.grafo.conecta( nomes[3], nomes[3] );
      GrafoTest.grafo.conecta( nomes[4], nomes[4] );
      Assert.assertFalse( GrafoTest.grafo.éConexo() );
      
      GrafoTest.grafo.conecta( nomes[0], "João" );
      GrafoTest.grafo.desconecta( nomes[1], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éConexo() );
      
      GrafoTest.grafo.conecta( nomes[1], nomes[4] );
      Assert.assertTrue( GrafoTest.grafo.éConexo() );
   }
   
   @Test
   public void testÉRegular() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomesArray = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomesArray );
      Assert.assertTrue( GrafoTest.grafo.éRegular() );
      
      GrafoTest.grafo.conecta( nomesArray[0], nomesArray[1] );
      Assert.assertFalse( GrafoTest.grafo.éRegular() );
      
      GrafoTest.grafo.conecta( nomesArray[2], nomesArray[3] );
      Assert.assertFalse( GrafoTest.grafo.éRegular() );
      
      GrafoTest.grafo.adicionaVértice( "Turquia" );
      GrafoTest.grafo.conecta( nomesArray[4], "Turquia" );
      Assert.assertTrue( GrafoTest.grafo.éRegular() );
   }
   
   @Test
   public void testEstãoConectados() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes );
      Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
   }
   
   @Test
   public void testFechoTransitivo1() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 1" );
      }
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 5, fechoTransitivoTeste.size() );
      Assert.assertEquals( 5, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo2() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 2" );
      }
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 5, fechoTransitivoTeste.size() );
      Assert.assertEquals( 5, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo3() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 3" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 6, fechoTransitivoTeste.size() );
      Assert.assertEquals( 6, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo4() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 4" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      GrafoTest.grafo.conecta( "João1", "João2" );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      fechoTransitivoModelo.add( "João2" );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 7, fechoTransitivoTeste.size() );
      Assert.assertEquals( 7, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo5() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 5" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      GrafoTest.grafo.conecta( "João1", "João2" );
      GrafoTest.grafo.conecta( "João2", "João3" );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      fechoTransitivoModelo.add( "João3" );
      fechoTransitivoModelo.add( "João2" );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 8, fechoTransitivoTeste.size() );
      Assert.assertEquals( 8, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo6() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 6" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      GrafoTest.grafo.conecta( "João1", "João2" );
      GrafoTest.grafo.conecta( "João2", "João3" );
      GrafoTest.grafo.conecta( "João4", "João5" );
      GrafoTest.grafo.conecta( "João5", "João6" );
      GrafoTest.grafo.conecta( "João7", "João8" );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      fechoTransitivoModelo.add( "João3" );
      fechoTransitivoModelo.add( "João2" );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 8, fechoTransitivoTeste.size() );
      Assert.assertEquals( 8, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo7() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 7" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      GrafoTest.grafo.conecta( "João1", "João2" );
      GrafoTest.grafo.conecta( "João2", "João3" );
      GrafoTest.grafo.conecta( "João4", "João5" );
      GrafoTest.grafo.conecta( "João5", "João6" );
      GrafoTest.grafo.conecta( "João7", "João8" );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( "João4" );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( "João4" );
      fechoTransitivoModelo.add( "João4" );
      fechoTransitivoModelo.add( "João6" );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 3, fechoTransitivoTeste.size() );
      Assert.assertEquals( 3, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo8() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 8" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      GrafoTest.grafo.conecta( "João1", "João2" );
      GrafoTest.grafo.conecta( "João2", "João3" );
      GrafoTest.grafo.conecta( "João4", "João5" );
      GrafoTest.grafo.conecta( "João5", "João6" );
      GrafoTest.grafo.conecta( "João7", "João8" );
      
      GrafoTest.grafo.conecta( "João6", "João7" );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( "João4" );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( "João4" );
      fechoTransitivoModelo.add( "João4" );
      fechoTransitivoModelo.add( "João6" );
      fechoTransitivoModelo.add( "João7" );
      fechoTransitivoModelo.add( "João8" );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 5, fechoTransitivoTeste.size() );
      Assert.assertEquals( 5, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testFechoTransitivo9() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 9" );
      }
      GrafoTest.grafo.adicionaVértice( "João1" );
      GrafoTest.grafo.adicionaVértice( "João2" );
      GrafoTest.grafo.adicionaVértice( "João3" );
      GrafoTest.grafo.adicionaVértice( "João4" );
      GrafoTest.grafo.adicionaVértice( "João5" );
      GrafoTest.grafo.adicionaVértice( "João6" );
      GrafoTest.grafo.adicionaVértice( "João7" );
      GrafoTest.grafo.adicionaVértice( "João8" );
      
      GrafoTest.grafo.conecta( "João1", nomes[0] );
      GrafoTest.grafo.conecta( "João1", "João2" );
      GrafoTest.grafo.conecta( "João2", "João3" );
      GrafoTest.grafo.conecta( "João4", "João5" );
      GrafoTest.grafo.conecta( "João5", "João6" );
      GrafoTest.grafo.conecta( "João7", "João8" );
      GrafoTest.grafo.conecta( "João6", "João7" );
      GrafoTest.grafo.conecta( "João8", nomes[3] );
      
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( "João4" );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( "João4" );
      fechoTransitivoModelo.add( "João2" );
      fechoTransitivoModelo.add( "João3" );
      fechoTransitivoModelo.add( "João4" );
      fechoTransitivoModelo.add( "João6" );
      fechoTransitivoModelo.add( "João7" );
      fechoTransitivoModelo.add( "João8" );
      fechoTransitivoModelo.add( nomes[0] );
      
      final Set< Object > temporário = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.addAll( temporário );
      
      final Iterator< Object > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 13, fechoTransitivoTeste.size() );
      Assert.assertEquals( 13, fechoTransitivoModelo.size() );
   }
   
   @Test
   public void testGrafoTempoMaisCedo() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      
      grafo.calcularOsTemposMaisCedo( "Vértice1" );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 3 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 1 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 2 ) ) );
   }
   
   @Test
   public void testGrafoTempoMaisCedo2() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      grafo.adicionaVértice( "Vértice5" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice2", "Vértice5", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      
      grafo.calcularOsTemposMaisCedo( "Vértice1" );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 3 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 1 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 2 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice5" ).equals( new Integer( 4 ) ) );
   }
   
   @Test
   public void testGrafoTempoMaisCedo3() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      grafo.adicionaVértice( "Vértice5" );
      grafo.adicionaVértice( "Vértice6" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice2", "Vértice5", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      grafo.calcularOsTemposMaisCedo( "Vértice1" );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 3 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 1 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 2 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice5" ).equals( new Integer( 4 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice6" ).equals( new Integer( 2 ) ) );
   }
   
   @Test
   public void testGrafoTempoMaisTarde() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      
      grafo.calcularOsTemposMaisTarde( "Vértice1", "Vértice2" );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 3 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 1 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 2 ) ) );
   }
   
   @Test
   public void testGrafoTempoMaisTarde2() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      grafo.adicionaVértice( "Vértice5" );
      grafo.adicionaVértice( "Vértice6" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice2", "Vértice5", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      grafo.calcularOsTemposMaisTarde( "Vértice1", "Vértice5" );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 3 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 1 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 2 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice5" ).equals( new Integer( 4 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice6" ).equals( new Integer( 3 ) ) );
   }
   
   @Test
   public void testGrafoTempoMaisTarde3() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      grafo.adicionaVértice( "Vértice5" );
      grafo.adicionaVértice( "Vértice6" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice2", "Vértice5", new Integer( 2 ) );
      grafo.conecta( "Vértice2", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      grafo.calcularOsTemposMaisTarde( "Vértice1", "Vértice5" );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 3 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 1 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 2 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice5" ).equals( new Integer( 5 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice6" ).equals( new Integer( 4 ) ) );
   }
   
   @Test
   public void testGrafoTemposDeAtrazoMaximo()
      throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      final Grafo temposMaisCedo = new Grafo( true );
      final Grafo temposMaisTarde = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      grafo.adicionaVértice( "Vértice5" );
      grafo.adicionaVértice( "Vértice6" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice2", "Vértice5", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      temposMaisCedo.adicionaVértice( "Vértice1" );
      temposMaisCedo.adicionaVértice( "Vértice2" );
      temposMaisCedo.adicionaVértice( "Vértice3" );
      temposMaisCedo.adicionaVértice( "Vértice4" );
      temposMaisCedo.adicionaVértice( "Vértice5" );
      temposMaisCedo.adicionaVértice( "Vértice6" );
      
      temposMaisCedo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice2", "Vértice5", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      temposMaisTarde.adicionaVértice( "Vértice1" );
      temposMaisTarde.adicionaVértice( "Vértice2" );
      temposMaisTarde.adicionaVértice( "Vértice3" );
      temposMaisTarde.adicionaVértice( "Vértice4" );
      temposMaisTarde.adicionaVértice( "Vértice5" );
      temposMaisTarde.adicionaVértice( "Vértice6" );
      
      temposMaisTarde.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice2", "Vértice5", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      temposMaisCedo.calcularOsTemposMaisCedo( "Vértice1" );
      temposMaisTarde.calcularOsTemposMaisTarde( "Vértice1", "Vértice5" );
      
      grafo.calcularOsTemposDeAtrazoMáximo( temposMaisCedo, temposMaisTarde );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice5" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice6" ).equals( new Integer( 1 ) ) );
   }
   
   @Test
   public void testGrafoTemposDeAtrazoMaximo2()
      throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final Grafo grafo = new Grafo( true );
      
      final Grafo temposMaisCedo = new Grafo( true );
      final Grafo temposMaisTarde = new Grafo( true );
      
      grafo.adicionaVértice( "Vértice1" );
      grafo.adicionaVértice( "Vértice2" );
      grafo.adicionaVértice( "Vértice3" );
      grafo.adicionaVértice( "Vértice4" );
      grafo.adicionaVértice( "Vértice5" );
      grafo.adicionaVértice( "Vértice6" );
      
      grafo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      grafo.conecta( "Vértice2", "Vértice5", new Integer( 2 ) );
      grafo.conecta( "Vértice2", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      grafo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      grafo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      grafo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      temposMaisCedo.adicionaVértice( "Vértice1" );
      temposMaisCedo.adicionaVértice( "Vértice2" );
      temposMaisCedo.adicionaVértice( "Vértice3" );
      temposMaisCedo.adicionaVértice( "Vértice4" );
      temposMaisCedo.adicionaVértice( "Vértice5" );
      temposMaisCedo.adicionaVértice( "Vértice6" );
      
      temposMaisCedo.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice2", "Vértice5", new Integer( 2 ) );
      temposMaisCedo.conecta( "Vértice2", "Vértice6", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      temposMaisCedo.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      temposMaisTarde.adicionaVértice( "Vértice1" );
      temposMaisTarde.adicionaVértice( "Vértice2" );
      temposMaisTarde.adicionaVértice( "Vértice3" );
      temposMaisTarde.adicionaVértice( "Vértice4" );
      temposMaisTarde.adicionaVértice( "Vértice5" );
      temposMaisTarde.adicionaVértice( "Vértice6" );
      
      temposMaisTarde.conecta( "Vértice1", "Vértice2", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice1", "Vértice3", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice2", "Vértice5", new Integer( 2 ) );
      temposMaisTarde.conecta( "Vértice2", "Vértice6", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice3", "Vértice6", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice3", "Vértice4", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice4", "Vértice2", new Integer( 1 ) );
      temposMaisTarde.conecta( "Vértice6", "Vértice5", new Integer( 1 ) );
      
      temposMaisCedo.calcularOsTemposMaisCedo( "Vértice1" );
      temposMaisTarde.calcularOsTemposMaisTarde( "Vértice1", "Vértice5" );
      
      grafo.calcularOsTemposDeAtrazoMáximo( temposMaisCedo, temposMaisTarde );
      
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice1" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice2" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice3" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice4" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice5" ).equals( new Integer( 0 ) ) );
      Assert.assertTrue( grafo.pesoDoVértice( "Vértice6" ).equals( new Integer( 0 ) ) );
   }
   
   @Test
   public void testHáCiclos() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      for( int i = 0; i < 25; i++ )
      {
         GrafoTest.grafo.adicionaVértice( Integer.valueOf( i ) );
      }
      GrafoTest.grafo.conecta( Integer.valueOf( 3 ), Integer.valueOf( 2 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 5 ), Integer.valueOf( 20 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 5 ), Integer.valueOf( 11 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 20 ), Integer.valueOf( 11 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 22 ), Integer.valueOf( 18 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 15 ), Integer.valueOf( 14 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 13 ), Integer.valueOf( 8 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 14 ), Integer.valueOf( 4 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 21 ), Integer.valueOf( 24 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 19 ), Integer.valueOf( 24 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 17 ), Integer.valueOf( 9 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 16 ), Integer.valueOf( 17 ) );
      GrafoTest.grafo.conecta( Integer.valueOf( 17 ), Integer.valueOf( 19 ) );
      Assert.assertTrue( GrafoTest.grafo.háCiclos() );
      
      GrafoTest.grafo.desconecta( Integer.valueOf( 5 ), Integer.valueOf( 11 ) );
      Assert.assertFalse( GrafoTest.grafo.háCiclos() );
   }
   
   @Test
   public void testPesoDoVérticeObject() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String vértice1 = "Vértice1";
      final String vértice2 = "Vértice2";
      
      final Integer peso1 = new Integer( 1 );
      final Integer peso2 = new Integer( 2 );
      
      GrafoTest.grafo.adicionaVértice( vértice1 );
      GrafoTest.grafo.adicionaVértice( vértice2 );
      
      GrafoTest.grafo.adicionarPesoAoVértice( vértice1, peso1 );
      
      Assert.assertTrue( GrafoTest.grafo.pesoDoVértice( vértice1 ).equals( peso1 ) );
      Assert.assertFalse( GrafoTest.grafo.pesoDoVértice( vértice1 ).equals( peso2 ) );
   }
   
   @Test
   public void testRemoverLaços() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      GrafoTest.grafo.removerLaços();
      
      final Set< Object > vértices = GrafoTest.grafo.vértices();
      final Object[] vérticesObjeto = vértices.toArray();
      
      for( int i = 0; i < 5; i++ )
      {
         Assert
            .assertFalse( GrafoTest.grafo.estãoConectados( vérticesObjeto[i], vérticesObjeto[i] ) );
      }
      Assert.assertEquals( 5, vértices.size() );
   }
   
   @Test
   public void testRemoverVértice() throws ExeçãoVérticeNãoExistente, ExeçãoVérticeJáExistente
   {
      final String[] nomes = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes );
      Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
      
      GrafoTest.grafo.removerVértice( nomes[1] );
      Assert.assertEquals( 4, GrafoTest.grafo.grau( nomes[0] ) );
   }
   
   @Test
   public void testUmVértice() throws ExeçãoVérticeJáExistente
   {
      final String[] nomesArray = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomesArray );
      final Object umVértice = GrafoTest.grafo.umVértice();
      Assert.assertTrue( GrafoTest.grafo.contémVertice( umVértice ) );
   }
   
   @Test
   public void testUmVérticeObject() throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomesArray = new String[] {
         "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértices( nomesArray );
      Object umVértice = GrafoTest.grafo.umVértice( nomesArray[0] );
      Assert.assertEquals( umVértice, nomesArray[0] );
      
      umVértice = GrafoTest.grafo.umVértice( nomesArray[2] );
      Assert.assertEquals( umVértice, nomesArray[2] );
      
      Exception exeção = null;
      try
      {
         GrafoTest.grafo.umVértice( "Cuba" );
         Assert.fail( "Exeção uma exeção deve ser lançada!" );
      }
      catch( final ExeçãoVérticeNãoExistente e )
      {
         exeção = e;
      }
      Assert.assertTrue( exeção instanceof ExeçãoVérticeNãoExistente );
   }
   
   @Test( expected = ExeçãoVérticeNãoExistente.class )
   public void testUmVérticeObjectExeption() throws ExeçãoVérticeNãoExistente
   {
      GrafoTest.grafo.umVértice( "Cuba" );
   }
}
