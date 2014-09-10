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
        
        /**
         * Método que retira todos os elementos da lista sem destruílos
         */
        void destroiLista() ;
        
        /**
         * 
         * @param tam
         */
        Lista<T>(int tam);
        void adiciona(T dado);
        void adicionaNoInicio(T dado);
        void adicionaNaPosicao(T dado, int posicao);
        void adicionaEmOrdem(T dado);
        T retira();
        T retiraDoInicio();
        T retiraDaPosicao(int posicao);
        T retiraEspecifico(T dado);
        int posicao(T dado);
        bool contem(T dado);
        bool igual(T dado1, T dado2);
        bool maior(T dado1, T dado2);
        bool menor(T dado1, T dado2);
        bool listaCheia();
        bool listaVazia();
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

template<typename T>
Lista<T>::Lista(int t)
{
	tamLista = t;
	dados = new T[tamLista];
	ultimo = -1;
}

template<typename T>
void Lista<T>::destroiLista()
{
	ultimo = -1;
}

template<typename T>
bool Lista<T>::listaCheia()
{
	if (ultimo == tamLista - 1)
		return true;
	return false;
}

template<typename T>
bool Lista<T>::listaVazia()
{
	if (ultimo == -1)
		return true;
	return false;
}

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

template<typename T>
bool Lista<T>::maior(T dado1, T dado2)
{
	if(dado1 > dado2)
		return true;
	return false;

}

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

template<typename T>
bool Lista<T>::igual(T dado1,T dado2)
{
	if(dado1 == dado2)
		return true;
	return false;

}

template<typename T>
bool Lista<T>::menor(T dado1,T dado2)
{
	if(dado1 < dado2)
		return true;
	return false;
}

#endif /* LISTA_HPP_ */
