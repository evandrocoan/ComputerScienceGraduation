//============================================================================
// Name        : ListaEncadeada.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <gtest/gtest.h>
#include "FilaEnc.hpp"
#include "testes/testesFeitosAMao.cpp"

using namespace std;

int main( int argc, char* argv[] )
{
    cout << "!!!Hello World!!!\n" << endl; // prints !!!Hello World!!!
    
    // variável de retorno do sucesso da execução do programa
    int a = 0;
    
    // chama o google test
    testing::InitGoogleTest( &argc, argv );
    a = RUN_ALL_TESTS();

    /*// chama os testes feitos a mão
    TestesFeitoAMao a;
    a.testePricipal();*/

    //getchar(); // keep console window open until Return keystroke
    
    
    return a;
}
;

class TesteListaEnc: public ::testing::Test
{
    protected:
        ListaEnc< int > lista;
};

TEST( sample_test_case, sample_test )
{
    EXPECT_EQ( 1, 1 );
}

TEST_F( TesteListaEnc, ListaVazia )
{
    ASSERT_TRUE( lista.listaVazia() );
}

TEST_F( TesteListaEnc, teste )
{
    lista.adicionaNaPosicao( 2, 0 );
    ASSERT_EQ( lista.tamanho(), 2 );
}
