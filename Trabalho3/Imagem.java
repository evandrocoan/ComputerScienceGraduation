// Image processing package in Java (a initial tentative)
// Prof. A. G. Silva - UFSC - June 2015

package Trabalho3;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Hashtable;

import javax.imageio.ImageIO;

import jpl.Atom;
import jpl.Query;
import jpl.Term;

public class Imagem
{
   
   public static int getB( final BufferedImage imagem, final int x, final int y )
   {
      final int V = imagem.getRGB( y, x );
      final int B = V & 255;
      return B;
   }
   
   public static int getG( final BufferedImage imagem, final int x, final int y )
   {
      final int V = imagem.getRGB( y, x );
      final int G = ( V >> 8 ) & 255;
      return G;
   }
   
   public static int getGray( final BufferedImage imagem, final int x, final int y )
   {
      final int R = Imagem.getR( imagem, x, y );
      final int G = Imagem.getG( imagem, x, y );
      final int B = Imagem.getB( imagem, x, y );
      final int Gray = (int) ( ( 0.2989 * R ) + ( 0.587 * G ) + ( 0.114 * B ) + 0.5 );
      return Gray;
   }
   
   public static int getR( final BufferedImage imagem, final int x, final int y )
   { // x linha, y coluna
      final int V = imagem.getRGB( y, x );
      final int R = ( V >> 16 ) & 255;
      return R;
   }
   
   public static void main( final String[] args )
   {
      BufferedImage imagem = null;
      try
      {
         // --------------------------------
         // LEITURA
         // --------------------------------
         final File imagemfile = new File( "C:\\local\\entrada.jpg" );
         imagem = ImageIO.read( imagemfile );
         
         // Exemplo de acesso ao pixel da linha 100 e coluna 150
         final int linha = 100;
         final int coluna = 150;
         int R = Imagem.getR( imagem, linha, coluna );
         int G = Imagem.getG( imagem, linha, coluna );
         int B = Imagem.getB( imagem, linha, coluna );
         final int Gray = Imagem.getGray( imagem, linha, coluna ); // nivel de cinza correspondente
         
         System.out.println( "(linha=" + linha + ", coluna=" + coluna + ") R=" + R + " G=" + G
            + " B=" + B + " Gray=" + Gray );
         
         // --------------------------------
         // MODIFICACAO (EXEMPLO: NEGATIVO)
         // --------------------------------
         int i, j, RGB;
         for( i = 0; i < imagem.getHeight(); i++ )
         {
            for( j = 0; j < imagem.getWidth(); j++ )
            {
               R = Imagem.getR( imagem, i, j );
               G = Imagem.getG( imagem, i, j );
               B = Imagem.getB( imagem, i, j );
               RGB = Imagem.setRGB( imagem, 255 - R, 255 - G, 255 - B );
               imagem.setRGB( j, i, RGB );
            }
         }
         
         // --------------------------------
         // GRAVACAO
         // --------------------------------
         ImageIO.write( imagem, "jpg", new File( "C:\\local\\saida.jpg" ) );
         
      } catch( final IOException e )
      {
         e.printStackTrace();
      }
      
      final Query q1 = new Query( "consult", new Term[] {
         new Atom( "c:/local/teste.pl" )
      } );
      
      System.out.println( "consult " + ( q1.query()? "succeeded" : "failed" ) );
      final Query q2 = new Query( "ancestral(X, jose)" );
      final Hashtable[] solution = q2.allSolutions();
      if( solution != null )
      {
         for( int i = 0; i < solution.length; i++ )
         {
            System.out.println( "X = " + solution[i].get( "X" ) );
         }
      }
      
   }
   
   public static int setRGB( final BufferedImage imagem, final int R, final int G, final int B )
   {
      final int RGB = ( 255 << 24 ) + ( R << 16 ) + ( G << 8 ) + B;
      return RGB;
   }
}
