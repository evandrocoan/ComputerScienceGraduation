//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/*
 * Lista.hpp
 *
 *  Created on: 04/09/2014
 *      Author: Carcara
 */

#ifndef LISTA_HPP_
#define LISTA_HPP_

#include <iostream>
using namespace std;

/**
 * Define o tamanho máximo da lista
 */
#define MAXLISTA 100

/**
 * Código do erro retornado pelo método quando a exceção Lista Cheia é lançada
 */
#define ERROLISTACHEIA -1

/**
 * Código do erro retornado pelo método quando a exceção Lista Vazia é lançada
 */
#define ERROLISTAVAZIA -2

/**
 * Código do errr retornado pelo método quando a é lançada a exceção onde
 * foi tentado acessar uma posição de memória fora dos limites do array
 */
#define ERROPOSICAO -3

/**
 * Classe que implementa um lista
 */
template<typename T>
class Lista
{
	private:
        /**
         * Atributo que representa o último elemento da lista
         */
        int ultimo;
        
        /**
         * Atributo que representa o tamanho máximo que a lista pode ter
         */
        int tamLista;
        
        /**
         * Atributo no qual se armazenam os dados da lista em forma de um 
         * vetor
         */
        T *dados;

	public:
        /**
         * Construtor padrão que cria uma lista vazia. O tamanho da lista 
         * é o valor definido na diretiva de compilação MAXLISTA
         */
        Lista();
        
        /**
         * Construtor que cria uma lista vazia do tamanho desejado.
         * 
         * @param tam um interio que representa o tamanho da lista a ser criada
         */
        Lista<T>(int tam);
        
        /**
         * Método que retira todos os elementos da lista sem destruílos
         */
        void destroiLista() ;
        
        /**
         * Informa se a lista chegou na capacidade máxima de elementos
         * 
         * @return true se a lista está cheira, false caso contrário
         */
        bool listaCheia();
        
        /**
         * Informa se a lista representada por este objetos está vazia
         * 
         * @return true se a lista está vazia, false caso contrário
         */
        bool listaVazia();
        
        /**
         * Adiciona um dado do tipo instanciado da lista.
         * 
         * @param dado do tipo instânciado a ser adicionado
         * @throws erro -1 caso o dado não possa ser adicionado
         */
        void adiciona(T dado);
        
        /**
         * Retira um dado da lista, assim abrindo uma nova posição
         * 
         * @return o dado retirado da lista, do mesmo tipo em que a lista foi
         * instânciada
         * @throws erro -2 caso a lista esteja vazia
         */
        T retira();
        
        /**
         * Adiciona um dado do tipo instânciado da lista no inicio dela
         * 
         * @param dado a ser adicionado no inicio da lista
         * @throw erro -1 caso a lista esteja cheia
         */
        void adicionaNoInicio(T dado);
        
        /**
         * Retira do dado do tipo da pilha do seu início
         * 
         * @return o dado retirado da pilha
         * @throw erro -2 caso a lista esteja vazia
         */
        T retiraDoInicio(); 
        
        /**
         * Adiciona um dado do tipo desta lista em um posição especicifica.
         * 
         * @param dado a ser adicionado na lista
         * @param posicao em que o dado será adicionado, de 0 à tamanho - 1
         * @throw erro '-1' caso a lista esteja cheia
         * @throw erro '-3' caso a posição seja inválida
         */
        void adicionaNaPosicao(T dado, int posicao);
        
        /**
         * Retira um elemento especificado como parâmetro e retorna ele do tipo
         * do objeto que esta lista representa.
         * 
         * @param posicao a ter o elemento retirado
         * @return o elemento retirado
         * @throw erro '-3' caso se tente retirar de uma posição inválida
         * @throw erro '-2' caso a lista esteja vazia
         */
        T retiraDaPosicao(int posicao);
        
        /**
         * Retorna um bool informando se o primeiro parâmetro é estritamente 
         * maior que o segundo.
         * 
         * @param dado1 do tipo desta lista para ser comparado com o segundo
         * @param dado2 do tipo desta lista para ser comprado com o primeiro
         * @return true se o primeiro parâmetro é maior que o segundo, retorna
         * false caso contrário
         */
        bool maior(T dado1, T dado2);
        
        /**
         * Adicina um dado do tipo desta lista na ordem sobrecarregada pelo 
         * tipo do objeto desta lista.
         * 
         * @param dado a ser adicionado em ordem na lista
         * @throw erro '-1' caso a lista esteja cheia
         */
        void adicionaEmOrdem(T dado);
        
        /**
         * Retorna um inteiro informando a posição do elemento do tipo que 
         * essa lista representa.
         * 
         * @param dado a ter a posição encontrada
         * @return a posição do elemento passado com parâmetro
         * @throw erro '-3' caso a posição a inserir não seja válida
         */
        int posicao(T dado);
        
        /**
         * Retira um objeto do tipo desta lista fornecido como parâmetro.
         * 
         * @param dado a ser retirado
         * @return o dado fornecido como parâmetro
         * @throw erro '-3' caso o dado não seja encontrado
         * @throw erro '-2' caso a lista esteja vazia
         */
        T retiraEspecifico(T dado); 
        
        /**
         * Verifica se o dado do tipo que esta lista representa está contido 
         * nesta lista.
         * 
         * @param dado a ser verificado a existência na lista
         * @return true caso a lista contenha o dado, false caso contrário
         * @throw erro '-2' caso a lista esteja vazia
         */
        bool contem(T dado);
        
        /**
         * Verifica se os dois dados do mesmo tipo dessa lista passados como 
         * parâmetros são iguais.
         * 
         * @param dado1 a ser verificado a igualdade
         * @param dado2 a ser verificado a igualdade
         * @return true se os dados são iguais, false caso contrário
         */
        bool igual(T dado1, T dado2);

        /**
         * Verifica se o primeiro parâmetro do mesmo tipo de dado que esta 
         * lista representa é estritamente menor que o segundo dado passado 
         * como parâmetro.
         * 
         * @param dado1 é o primeiro dado a ser verificado
         * @param dado2 e o segundo dado a ser verificado
         * @return true se o primeiro parâmetro é estritamente menor que o 
         * segundo parâmetro
         */
        bool menor(T dado1, T dado2);

};

/**
 * \copydoc Lista<T>::Lista()
 */
template<typename T>
Lista<T>::Lista()
{
	tamLista = MAXLISTA;
	dados = new T[tamLista];
	ultimo = -1;
}

/**
 * \copydoc Lista<T>::Lista(int t)
 */
template<typename T>
Lista<T>::Lista(int t)
{
	tamLista = t;
	dados = new T[tamLista];
	ultimo = -1;
}

/**
 * \copydoc Lista<T>::destroiLista()
 */
template<typename T>
void Lista<T>::destroiLista()
{
	ultimo = -1;
}

/**
 * \copydoc Lista<T>::listaCheia()
 */
template<typename T>
bool Lista<T>::listaCheia()
{
	if (ultimo == tamLista - 1)
		return true;
	return false;
}

/**
 * \copydoc Lista<T>::listaVazia()
 */
template<typename T>
bool Lista<T>::listaVazia()
{
	if (ultimo == -1)
		return true;
	return false;
}

/**
 * \copydoc Lista<T>::adiciona(T dado)
 */
template<typename T>
void Lista<T>::adiciona(T dado)
{
	if (this->listaCheia())
	{
		throw ERROLISTACHEIA;
	}
	else
	{
		ultimo++;
		dados[ultimo] = dado;
		//return ultimo;
	}

}

/**
 * \copydoc Lista<T>::retira()
 */
template<typename T>
T Lista<T>::retira()
{
	if (this->listaVazia())
		throw ERROLISTAVAZIA;
	else
	{
		ultimo--;
		return (dados[ultimo + 1]);
	}

}

/**
 * \copydoc Lista<T>::adicionaNoInicio(T dado)
 */
template<typename T>
void Lista<T>::adicionaNoInicio(T dado)
{
	int posicao;

	if (this->listaCheia())
		throw ERROLISTACHEIA;
	else
	{
		ultimo++;
		posicao = ultimo;
		while (posicao > 0)
		{
			dados[posicao] = dados[posicao - 1];
			posicao--;
		}
		dados[0] = dado;
		//return 0;
	}
}

/**
 * \copydoc Lista<T>::retiraDoInicio()
 */
template<typename T>
T Lista<T>::retiraDoInicio()
{
	int posicao, valor;

	if(this->listaVazia())
	{
		throw ERROLISTAVAZIA;
	}
	else
	{
		ultimo--;
		valor = dados[0];
		posicao=0;
		while(posicao<=ultimo)
		{
			dados[posicao] = dados[posicao + 1];
			posicao++;
		}
		return valor;
	}
}

/**
 * \copydoc Lista<T>::adicionaNaPosicao(T dado, int destino)
 */
template<typename T>
void Lista<T>::adicionaNaPosicao(T dado, int destino)
{
	int posicao;

	if(this->listaCheia())
		throw ERROLISTACHEIA;
	else
	{
		if((destino > (ultimo + 1)) || (destino < 0))
			throw ERROPOSICAO;
		ultimo++;
		posicao=ultimo;
		while(posicao>destino)
		{
			 dados[posicao] = dados[posicao - 1];
			 posicao--;
		}
		 dados[destino] = dado;
		 //return destino;
	}
}

/**
 * \copydoc Lista<T>::retiraDaPosicao(int fonte)
 */
template<typename T>
T Lista<T>::retiraDaPosicao(int fonte)
{
	int posicao, valor;

	if((fonte > ultimo) || (fonte<0))
		throw ERROPOSICAO;
	else
	{
		if(this->listaVazia())
			throw ERROLISTAVAZIA;
		else
		{
			ultimo--;
			valor=dados[fonte];
			posicao = fonte;
			while(posicao<=ultimo)
			{
				dados[posicao]=dados[posicao+1];
				posicao++;
			}
			return valor;
		}
	}
}

/**
 * \copydoc Lista<T>::maior(T dado1, T dado2)
 */
template<typename T>
bool Lista<T>::maior(T dado1, T dado2)
{
	if(dado1 > dado2)
		return true;
	return false;

}

/**
 * \copydoc Lista<T>::adicionaEmOrdem(T dado)
 */
template<typename T>
void Lista<T>::adicionaEmOrdem(T dado)
{
	int posicao;

	if(this-> listaCheia())
		throw ERROLISTACHEIA;
	else
	{
		posicao = 0;
		while((posicao<= ultimo)&&(dado>dados[posicao]))
			posicao++;
		return this->adicionaNaPosicao(dado,posicao);
	}

}

/**
 * \copydoc Lista<T>::posicao(T dado)
 */
template<typename T>
int Lista<T>::posicao(T dado)
{
	int posicao=0;

	while((posicao<=ultimo) && !(igual(dado, dados[posicao])))
		posicao++;
	if(posicao>ultimo)
		throw ERROPOSICAO;
	else
		return posicao;
}

/**
 * \copydoc Lista<T>::retiraEspecifico(T dado)
 */
template<typename T>
T Lista<T>::retiraEspecifico(T dado)
{
	int posicao;

	if(this->listaVazia())
		throw ERROLISTAVAZIA;
	else
	{
		posicao = this->posicao(dado);
		if(posicao<0)
			throw ERROPOSICAO;
		else
			return this->retiraDaPosicao(posicao);
	}

}

/**
 * \copydoc Lista<T>::contem(T dado)
 */
template<typename T>
bool Lista<T>::contem(T dado)
{
	if(this-> listaVazia())
		throw ERROLISTAVAZIA;
	else
	{
		for(int i=0; i<=ultimo; i++)
		{
			if(this->igual(dados[i],dado))
				return true;
		}
	}
	return false;


}

/**
 * \copydoc Lista<T>::igual(T dado1,T dado2)
 */
template<typename T>
bool Lista<T>::igual(T dado1,T dado2)
{
	if(dado1 == dado2)
		return true;
	return false;

}

/**
 * \copydoc Lista<T>::menor(T dado1,T dado2)
 */
template<typename T>
bool Lista<T>::menor(T dado1,T dado2)
{
	if(dado1 < dado2)
		return true;
	return false;
}

#endif /* LISTA_HPP_ */
