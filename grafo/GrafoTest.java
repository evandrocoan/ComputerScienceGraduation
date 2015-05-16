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
    * Resposável por realizar o debug do programa.
    */
   static final Logger LOG = Logger.getLogger( GrafoTest.class.getName() );
   
   private static Grafo grafo;
   
   @BeforeClass
   public static void configureLOG()
   {
      GrafoTest.LOG.setLevel( Level.INFO );
   }
   
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
   
   @After
   public void logResults()
   {
      GrafoTest.LOG.info( GrafoTest.grafo.toString() + "\n " );
   }
   
   /**
    * É executado antes que um teste inicia. Os testes execuram na seguinte
    * ordem: setUp(), test1(), printBye(), setUp(), test2(), printBye()...
    */
   @Before
   public void setUp()
   {
      GrafoTest.grafo = new Grafo();
   }
   
   @Test
   public void testAdicionarVértice() throws ExeçãoVérticeNãoExistente,
            ExeçãoVérticeJáExistente
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
      final Vector< String > adjacentesVector = new Vector<>();
      adjacentesVector.add( nomes[1] );
      adjacentesVector.add( nomes[4] );
      final Enumeration< ? > adjacentes = adjacentesVector.elements();
      
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.adicionaVértice( "França", adjacentes );
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
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes );
      Assert.assertEquals( 5, GrafoTest.grafo.grau( nomes[0] ) );
      
      GrafoTest.grafo.adicionaVértice( "Tcheca" );
      GrafoTest.grafo.conecta( "Tcheca", nomes[0] );
      Assert.assertEquals( 6, GrafoTest.grafo.grau( nomes[0] ) );
      
      GrafoTest.grafo.conecta( "Tcheca", "Tcheca" );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "Tcheca" ) );
   }
   
   @Test
   public void testAdjacentes() throws ExeçãoVérticeNãoExistente,
            ExeçãoVérticeJáExistente
   {
      final String[] nomesArray = new String[] {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomesArray );
      GrafoTest.grafo.conecta( nomesArray[0], nomesArray );
      
      final Vector< String > nomesVetor = new Vector<>(
               Arrays.asList( nomesArray ) );
      final Iterator< String > nomesIterador = nomesVetor.iterator();
      
      final Collection< ? > adjacentes = GrafoTest.grafo.adjacentes( nomesArray[0] );
      
      while( nomesIterador.hasNext() )
      {
         final Object objeto = nomesIterador.next();
         Assert.assertTrue( adjacentes.contains( objeto ) );
      }
      Assert.assertTrue( 5 == GrafoTest.grafo.ordem() );
   }
   
   @Test
   public void testDesconectarVérticesObjectObject()
            throws ExeçãoVérticeJáExistente, ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      final Vector< String > adjacentes = new Vector<>();
      adjacentes.add( nomes[1] );
      adjacentes.add( nomes[4] );
      
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.adicionaVértice( "França", adjacentes );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
      
      GrafoTest.grafo.adicionaVértice( "Tcheca" );
      GrafoTest.grafo.conecta( "Tcheca", "França" );
      Assert.assertEquals( 3, GrafoTest.grafo.grau( "França" ) );
      
      GrafoTest.grafo.desconecta( "Tcheca", "França" );
      Assert.assertEquals( 2, GrafoTest.grafo.grau( "França" ) );
      Assert.assertEquals( 0, GrafoTest.grafo.grau( "Tcheca" ) );
   }
   
   @Test
   public void testÉÁrvore1() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      /* TODO @formatter:off
		 * 
		 * 
		 */ // @formatter:on 
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      Assert.assertFalse( GrafoTest.grafo.éÁrvore() );
   }
   
   @Test
   public void testÉÁrvore2() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
   public void testÉÁrvore3() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
   public void testÉÁrvore4() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
   public void testÉCompleto() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
   public void testÉConexo() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
   public void testÉRegular() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomesArray = new String[] {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomesArray );
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
   public void testEstãoConectados() throws ExeçãoVérticeNãoExistente,
            ExeçãoVérticeJáExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.conecta( nomes[0], nomes );
      Assert.assertTrue( GrafoTest.grafo.estãoConectados( nomes[0], "USA" ) );
   }
   
   @Test
   public void testFechoTransitivo1() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 1" );
      }
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 5, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo2() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      
      if( GrafoTest.LOG.isLoggable( Level.FINE ) )
      {
         System.err.println( "Parte 2" );
      }
      final Set< Object > fechoTransitivoTeste = GrafoTest.grafo.fechoTransitivo( nomes[0] );
      
      final Set< Object > fechoTransitivoModelo = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.add( nomes[0] );
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
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
   }
   
   @Test
   public void testFechoTransitivo3() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 6, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo4() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 7, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo5() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 8, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo6() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 8, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo7() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 3, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo8() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 5, fechoTransitivoTeste.size() );
   }
   
   @Test
   public void testFechoTransitivo9() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      fechoTransitivoModelo.add( "João4" );
      fechoTransitivoModelo.add( "João6" );
      fechoTransitivoModelo.add( "João7" );
      fechoTransitivoModelo.add( "João8" );
      
      final Set< Object > temporário = GrafoTest.grafo.adjacentes( nomes[0] );
      fechoTransitivoModelo.addAll( temporário );
      
      final Iterator< ? > iteradorModelo = fechoTransitivoModelo.iterator();
      
      while( iteradorModelo.hasNext() )
      {
         final Object próximo = iteradorModelo.next();
         Assert.assertTrue( fechoTransitivoTeste.contains( próximo ) );
      }
      Assert.assertEquals( 13, fechoTransitivoTeste.size() );
   }
   
   @Test
   @SuppressWarnings( "boxing" )
   public void testHáCiclos() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   { // TODO
      final int[] números = new int[25];
      for( int i = 0; i < 25; i++ )
      {
         números[i] = i;
      }
      GrafoTest.grafo.adicionaVértice( números );
      
      GrafoTest.grafo.conecta( 3, 2 );
      GrafoTest.grafo.conecta( 5, 20 );
      GrafoTest.grafo.conecta( 5, 11 );
      GrafoTest.grafo.conecta( 20, 11 );
      GrafoTest.grafo.conecta( 22, 18 );
      GrafoTest.grafo.conecta( 15, 14 );
      GrafoTest.grafo.conecta( 13, 8 );
      GrafoTest.grafo.conecta( 14, 4 );
      GrafoTest.grafo.conecta( 21, 24 );
      GrafoTest.grafo.conecta( 19, 24 );
      GrafoTest.grafo.conecta( 17, 9 );
      GrafoTest.grafo.conecta( 16, 17 );
      GrafoTest.grafo.conecta( 17, 19 );
      
      Assert.assertTrue( GrafoTest.grafo.háCiclos() );
   }
   
   @Test
   public void testRemoverLaços() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomes = {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
      GrafoTest.grafo.conecta( nomes, nomes );
      GrafoTest.grafo.removerLaços();
      
      final Set< ? > vértices = GrafoTest.grafo.vértices();
      final Object[] vérticesObjeto = vértices.toArray();
      
      for( int i = 0; i < 5; i++ )
      {
         Assert.assertFalse( GrafoTest.grafo.estãoConectados(
                  vérticesObjeto[i], vérticesObjeto[i] ) );
      }
      Assert.assertEquals( 5, vértices.size() );
   }
   
   @Test
   public void testRemoverVértice() throws ExeçãoVérticeNãoExistente,
            ExeçãoVérticeJáExistente
   {
      final String[] nomes = new String[] {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomes );
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
      GrafoTest.grafo.adicionaVértice( nomesArray );
      final Object umVértice = GrafoTest.grafo.umVértice();
      Assert.assertTrue( GrafoTest.grafo.contémVertice( umVértice ) );
   }
   
   @Test
   public void testUmVérticeObject() throws ExeçãoVérticeJáExistente,
            ExeçãoVérticeNãoExistente
   {
      final String[] nomesArray = new String[] {
               "Brasil", "USA", "China", "Hong Kong", "Japão"
      };
      GrafoTest.grafo.adicionaVértice( nomesArray );
      Object umVértice = GrafoTest.grafo.umVértice( nomesArray[0] );
      Assert.assertEquals( umVértice, nomesArray[0] );
      
      umVértice = GrafoTest.grafo.umVértice( nomesArray[2] );
      Assert.assertEquals( umVértice, nomesArray[2] );
      
      Exception exeção = null;
      try
      {
         GrafoTest.grafo.umVértice( "Cuba" );
         Assert.fail( "Exeção uma exeção deve ser lançada!" );
      } catch( final ExeçãoVérticeNãoExistente e )
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
