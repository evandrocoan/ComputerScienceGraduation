/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho7;

/**
 *
 * @author Professional
 */
public class Pessoa 
{
   private String nome;
   private int idade;
   private char sexo;
   
   public Pessoa( String nome, char sexo, int idade )
   {
      this.nome = nome;
      this.sexo = Character.toUpperCase(sexo);
      if( sexo != 'M' && sexo != 'F') { this.sexo = '*'; }
      this.idade = idade;
      if( idade < 0 || idade > 120 ) { idade = 0; }
      
   }

   /**
    * @return the nome
    */
   public String getNome() 
   {
      return nome;
      
   }

   /**
    * @return the idade
    */
   public int getIdade() 
   {
      return idade;
      
   }

   /**
    * @return the sexo
    */
   public char getSexo() 
   {
      return sexo;
      
   }

   /**
    * @param nome the nome to set
    */
   public void setNome(String nome) 
   {
      this.nome = nome;
      
   }

   /**
    * @param idade the idade to set
    */
   public void setIdade(int idade) 
   {
      if( idade < 0 || idade > 120 ) 
      { 
         this.idade = 0; 
         
      } else
      {
         this.idade = idade;
         
      }
      
   }

   /**
    * @param sexo the sexo to set
    */
   public void setSexo(char sexo) 
   {
      this.sexo = Character.toUpperCase(sexo);
      if( sexo != 'M' && sexo != 'F') { sexo = '*'; }
      
   }
   
   public String paraString()
   {
      return "Nome: " + nome + "\nSexo " + "sexo " + "\nIdade " + idade;
      
   }
	
} // end class Pessoa
