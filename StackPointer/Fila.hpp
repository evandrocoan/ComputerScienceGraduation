/**
 * Implementa uma fila usando Templates. E com um numero de elementos 
 * variavel definido na instanciacao.
 * 
 * @authors Charles B. L. e Evandro Coan -  Grupo 4
 */

#include <iostream>

using namespace std;

/**
 * Codigo de erro para uma fila cheia
 */
#define ERRO_FILA_CHEIA -1

/**
 * Codigo de erro para uma fila vazia
 */
#define ERRO_FILA_VAZIA -2

/**
 * Indica qual o tamnha padrao para a fila
 */
#define TAMANHO_PADRAO 30

template<typename T>
class Fila
{
   private:
      /**
       * Vetor que encapsula a fila
       */
      T *fila;
      
      /**
       * Indica o final da fila
       */
      int finalDaFila;
      
      /**
       * Armazena o tamanho da fila
       */
      int tamanhoDaFila;
      
   public:
      /**
       * Construtor que cria uma fila com o tamanho padrao definido na
       * diretiva TAMANHO_PADRAO
       */
      Fila();
      
      /**
       * Construtor que cria uma fila com o tamanho enviado como argumento
       * 
       * @param tam o tamanho da fila a ser criada
       */
      Fila<T>(int tam);
      
      /**
       * Coloca um dado passado como argumento na fila
       * @param dado o dado a ser inserido na fila
       */
      void inclui(T dado);
      
      /**
       * Retira o primeiro dado da fila e retorna ele
       * 
       * @return dado retirado da fila
       */
      T retira();
      
      /**
       * Retorna o ultimo dado colocado na fila
       * 
       * @return dado o ultimo dado adicionado na fila
       */
      T ultimo();
      
      /**
       * Serve para saber qual a posicao do final da fila
       * 
       * @return posicao a posicao do ultimo elemento na fila
       */
      int getUltimo();
      
      /**
       * Informa se a fila esta cheia
       * 
       * @return um boolean true caso sim, um boolean false caso nao
       */
      bool filaCheia();
      
      /**
       * Informa se a fila esta vazia retornando uma variavel booleana
       * 
       * @return boolean true caso esteja vazia, boolean false caso contenha
       * elementos
       */
      bool filaVazia();
      
      /**
       * Descarta toda a fila e iniciase uma nova fila como definida no 
       * construtor padrão
       */
      void inicializaFila();
};

/** 
 * \copydoc Fila::Fila()
 */
template <typename T>
Fila<T>::Fila()
{
   this -> inicializaFila();
}

/**
 * \copydoc Fila::Fila(int tam)
 */
template<typename T>
Fila<T>::Fila(int tam)
{
   this -> finalDaFila = -1;
   this -> tamanhoDaFila = tam;
   fila = new T[this -> tamanhoDaFila];
}

/**
 * \copydoc Fila::inclui(T dado)
 */
template<typename T>
void Fila<T>::inclui(T dado)
{
   try
   {
      if(this -> filaCheia())
      {
         throw ERRO_FILA_CHEIA;
      } else 
      {
         this -> finalDaFila++;
         this -> fila[this -> finalDaFila] = dado;
      }
   } catch(int codigo)
   {
      cout << "Erro no metodo inclui - Fila Cheia " << codigo << endl;
      throw ERRO_FILA_CHEIA;
   }
}

/**
 * \copydoc Fila<T>::retira()
 */
template<typename T>
T Fila<T>::retira()
{
   try
   {
      if(this -> filaVazia())
      {
         throw ERRO_FILA_VAZIA;
      } else
      {
         T retorno = fila[0];
         
         int i;
         for( i = 0 ; i < this -> finalDaFila; i++)
         {
            fila[i] = fila[i+1];
         }
         
         this -> finalDaFila--;
         return retorno;
      }
   }catch(int codigo)
   {
      cout << "Erro ao retirar - Fila Vazia " << codigo << endl;
      throw ERRO_FILA_VAZIA;
   }
}

/**
 * \copydoc Fila<T>::ultimo()
 */
template<typename T>
T Fila<T>::ultimo()
{
   try
   {
      if(this -> filaVazia())
      {
         throw ERRO_FILA_VAZIA;
      } else
      {
         return this -> fila[this -> finalDaFila];
      }
   } catch(int codigo)
   {
      cout << "Erro ao obter o ultimo elemento - Fila Vazia " << codigo 
              << endl;
      throw ERRO_FILA_VAZIA;
   }
}

/**
 * \copydoc Fila<T>::getUltimo()
 */
template<typename T>
int Fila<T>::getUltimo()
{
   try
   {
		if(this -> filaVazia())
      {
			throw ERRO_FILA_VAZIA;
      }
		else
      {
			return this -> finalDaFila;
      }
	}
	catch(int codigo)
   {
		cout << "Erro - Fila vazia " << codigo << endl;
		throw ERRO_FILA_VAZIA;
	}
}

/**
 * \copydoc Fila<T>::filaCheia()
 */
template<typename T>
bool Fila<T>::filaCheia()
{
   if(this -> finalDaFila >= (this -> tamanhoDaFila - 1) )
   {
		return true;
   }
	return false;
}

/**
 * \copydoc Fila<T>::filaVazia()
 */
template<typename T>
bool Fila<T>::filaVazia()
{
   if( this -> finalDaFila < 0 )
   {
		return true;
   }
	return false;
}

/**
 * \copydoc Fila<T>::inicializaFila()
 */
template<typename T>
void Fila<T>::inicializaFila()
{
   this -> finalDaFila = -1;
   this -> tamanhoDaFila = TAMANHO_PADRAO;
   this -> fila = new T[this -> tamanhoDaFila];
}
