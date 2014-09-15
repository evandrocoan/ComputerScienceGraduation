/*
 * ListaEnc.hpp
 *
 *  Created on: 13/09/2014
 *      Author: Carcara
 */

#ifndef LISTAENC_HPP_
#define LISTAENC_HPP_

#include "Elemento.hpp"

template<typename T>
class ListaEnc
{
	private:
		Elemento<T>* head;		//ponteiro para o primeiro elemento da lista
		int size;				//numero de elementos da lista

	public:
		ListaEnc();
		~ListaEnc();

		// inicio
		void adicionaNoInicio(const T& dado);
		T retiraDoInicio();
		void eliminaDoInicio();

		// posicao
		void adicionaNaPosicao(const T& dado, int pos);
		int posicao(const T& dado) const;
		T* posicaoMem(const T& dado) const;
		bool contem(const T& dado);
		T retiraDaPosicao(int pos);

		//fim
		void adiciona(const T& dado);
		T retira();

		// especifico
		T retiraEspecifico(const T& dado);
		void adicionaEmOrdem(const T& data);
		bool listaVazia() const;
		bool igual(T dado1, T dado2);
		bool maior(T dado1, T dado2);
		bool menor(T dado1, T dado2);
		void destroiLista();
};

//-----------------------------------------------------------------
template<typename T>
ListaEnc<T>::ListaEnc()
{
	size = 10;			//CORRIGIR
	head = new T[size]; //CORRIGIR
}

Lista* MÉTODO criaLista()
//Retorna ponteiro para uma nova cabeça de lista ou NULO.
  variáveis
     Lista *aLista;
início
     aLista <- aloque(Lista);
SE (aLista ~= NULO) ENTÃO
//Só posso inicializar se consegui alocar.
        aLista->tamanho <- 0;
        aLista->dados <- NULO;
     FIM SE
     RETORNE(aLista);
  fim;
//-----------------------------------------------------------------
template<typename T>
ListaEnc<T>::~ListaEnc()
{}
//-----------------------------------------------------------------
template<typename T>
void ListaEnc<T>::adicionaNoInicio(const T& dado)
{

}
//-----------------------------------------------------------------
template<typename T>
		T retiraDoInicio();
//-----------------------------------------------------------------
template<typename T>
void ListaEnc<T>::eliminaDoInicio()
{

}
//-----------------------------------------------------------------
template<typename T>
void ListaEnc<T>::adicionaNaPosicao(const T& dado, int pos)
{

}
//-----------------------------------------------------------------
template<typename T>
int ListaEnc<T>::posicao(const T& dado) const
{

}
//-----------------------------------------------------------------
template<typename T>
T* ListaEnc<T>::posicaoMem(const T& dado) const
{

}
//-----------------------------------------------------------------
template<typename T>
bool ListaEnc<T>::contem(const T& dado)
{

}
//-----------------------------------------------------------------
template<typename T>
T ListaEnc<T>::retiraDaPosicao(int pos)
{

}
//-----------------------------------------------------------------
template<typename T>
void ListaEnc<T>::adiciona(const T& dado)
{

}
//-----------------------------------------------------------------
template<typename T>
T ListaEnc<T>::retira()
{

}
//-----------------------------------------------------------------
template<typename T>
T ListaEnc<T>::retiraEspecifico(const T& dado)
{

}
//-----------------------------------------------------------------
template<typename T>
void ListaEnc<T>::adicionaEmOrdem(const T& data)
{

}
//-----------------------------------------------------------------
template<typename T>
bool ListaEnc<T>::listaVazia() const
{

}
//-----------------------------------------------------------------
template<typename T>
bool ListaEnc<T>::igual(T dado1, T dado2)
{

}
//-----------------------------------------------------------------
template<typename T>
bool ListaEnc<T>::maior(T dado1, T dado2)
{

}
//-----------------------------------------------------------------
template<typename T>
bool ListaEnc<T>::menor(T dado1, T dado2)
{

}
//-----------------------------------------------------------------
template<typename T>
void ListaEnc<T>::destroiLista()
{

}
//-----------------------------------------------------------------
#endif /* LISTAENC_HPP_ */
