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

#define MAXLISTA 100
#define ERROLISTACHEIA -1
#define ERROLISTAVAZIA -2
#define ERROPOSICAO -3

template<typename T>
class Lista
{
	private:
		int ultimo;
		int tamLista;
		T *dados;

	public:
		Lista();
		Lista<T>(int t);
		void destroiLista();
		bool listaCheia();
		bool listaVazia();
		int adiciona(T dado);
		T retira();
		int adicionaNoInicio(T dado);
		T retiraDoInicio();
		int adicionaNaPosicao(T dado, int posicao);
		T retiraDaPosicao(int posicao);
		bool maior(T dado1,T dado2);
		int  adicionaEmOrdem(T dado);
		int posicao(T dado);
		T  retiraEspecifico(T dado);
		bool contem(T dado);
		bool igual(T dado1,T dado2);
		bool menor(T dado1,T dado2);

};

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
	if (ultimo == MAXLISTA - 1)
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
int Lista<T>::adiciona(T dado)
{
	if (this->listaCheia())
	{
		throw ERROLISTACHEIA;
	}
	else
	{
		ultimo++;
		dados[ultimo] = dado;
		return ultimo;
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
		return (dados(ultimo + 1));
	}

}

template<typename T>
int Lista<T>::adicionaNoInicio(T dado)
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
		return 0;
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
int Lista<T>::adicionaNaPosicao(T dado, int destino)
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
		 return destino;
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
int Lista<T>::adicionaEmOrdem(T dado)
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


//metodos nossos
template<typename T>
bool Lista<T>::contem(T dado)
{

}

template<typename T>
bool Lista<T>::igual(T dado1,T dado2)
{

}

template<typename T>
bool Lista<T>::menor(T dado1,T dado2)
{

}

#endif /* LISTA_HPP_ */
