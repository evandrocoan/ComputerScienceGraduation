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
#define ERROLISTACHEIA -1;
#define ERROLISTAVAZIA -2;
#define ERROPOSIÇÃO -3;

template<typename T>
class Lista
{
	private:
		int ultimo;		
		int tamLista;
		T *lista;

	public:
		Lista();
		Lista<T>(int t);
		void destroiLista();
		bool listaCheia();
		bool listaVazia();
		///**********************


		void AdicionaNoInício(T dado);
		void AdicionaNaPosição(T dado, int posicao); 
		void  AdicionaEmOrdem(T dado);
		void Retira();
		void RetiraDoInício();
		void RetiraDaPosição(int posicao);
		void  RetiraEspecífico(T dado);
		


		int Posicao(T dado);
		bool Contem(T dado);
		bool Igual(T dado1,T dado2); 
		bool Maior(T dado1,T dado2);
		bool Menor(T dado1,T dado2);
		
	
};

template <typename T>
Lista<T>::Lista(){
	tamLista = MAXLISTA;
	lista = new T[tamLista];
	ultimo = -1;
}

template <typename T>
Lista<T>::Lista(int t){
	tamLista = t;
	lista = new T[tamLista];
	ultimo = -1;
}

template <typename T>
void Lista<T>::destroiLista()
{
	ultimo = -1;
}

template <typename T>
bool Lista<T>::listaCheia()
{
	if(ultimo==MAXLISTA-1)
		return true;
	return false;
}

template <typename T>
bool Lista<T>::listaVazia()
{
	if(ultimo==-1)
		return true;
	return false;
}


#endif /* LISTA_HPP_ */
