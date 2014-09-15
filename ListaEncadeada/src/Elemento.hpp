/*
 * Elemento.hpp
 *
 *  Created on: 13/09/2014
 *      Author: Carcara
 */

#ifndef ELEMENTO_HPP_
#define ELEMENTO_HPP_

template<typename T>
class Elemento
{
	private:
		T *info;				//ponteiro para o tipo de informação que vai ser armazenada
		Elemento<T>* _next;		//ponteiro para o próximo elemento da lista

	public:
		Elemento(const T& info, Elemento<T>* next) : info(new T(info)), _next(next) {}

	//-----------------------------------------------------------------
	~Elemento()
	{
		delete info;
	}
	//-----------------------------------------------------------------
	Elemento<T>* getProximo() const
	{
		return _next;
	}
	//-----------------------------------------------------------------
	T getInfo() const
	{
		return *info;
	}
	//-----------------------------------------------------------------
	void setProximo(Elemento<T>* next)
	{
		_next = next;
	}
	//-----------------------------------------------------------------
};

#endif /* ELEMENTO_HPP_ */
