/*
 Copyright [2014] <Jean Martina>
 */
#include <cstdlib>
#include <ctime>
#include "gtest/gtest.h"
#include "ListaEnc.hpp"
#define tam 10
#define offset 1000

int main( int argc, char* argv[] )
{
    std::srand( std::time( NULL ) );
    ::testing::InitGoogleTest( &argc, argv );
    return RUN_ALL_TESTS();
}

class Objeto
{
};

class TesteLista: public ::testing::Test
{
    protected:
        ListaEnc< int > l;
        ListaEnc< Objeto* > lobj;
};

TEST_F(TesteLista, ListaVazia)
{
    ASSERT_TRUE( l.listaVazia() );
    ASSERT_TRUE( lobj.listaVazia() );
}

TEST_F(TesteLista, ListaVaziaNeg)
{
    l.adiciona( 1 );
    ASSERT_FALSE( l.listaVazia() );
    lobj.adiciona( new Objeto );
    ASSERT_FALSE( lobj.listaVazia() );
}

TEST_F(TesteLista, ListaContem)
{
    Objeto* o = new Objeto();
    l.adiciona( 1 );
    ASSERT_TRUE( l.contem( 1 ) );
    lobj.adiciona( o );
    ASSERT_TRUE( lobj.contem( o ) );
}

TEST_F(TesteLista, ListaContemNeg)
{
    Objeto* o = new Objeto();
    Objeto* o2 = new Objeto();
    l.adiciona( 1 );
    ASSERT_FALSE( l.contem( 2 ) );
    lobj.adiciona( o );
    ASSERT_FALSE( lobj.contem( o2 ) );
}

TEST_F(TesteLista, ListaDestroi)
{
    l.adiciona( 1 );
    l.destroiLista();
    ASSERT_TRUE( l.listaVazia() );
}

TEST_F(TesteLista, ListaPosicao)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_EQ( l.posicao( 5 ), 5 );
}

TEST_F(TesteLista, ListaPosicaoExcep)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_ANY_THROW( l.posicao(tam+2) );
}

/*TEST_F(TesteLista, ListaAdiciona) {
 int i;
 for (i = 0; i < tam ; i++) {
 l.adiciona(i);
 }
 ASSERT_ANY_THROW(l.adiciona(i+1));
 }*/

TEST_F(TesteLista, ListaAdicionaInicio)
{
    int i;
    for( i = 0; i < tam - 1; i++ )
    {
        l.adicionaNoInicio( i );
    }
    l.adicionaNoInicio( i + 1 );
    ASSERT_EQ( l.posicao( i + 1 ), 0 );
}

/*TEST_F(TesteLista, ListaAdicionaExcep) {
 int i;
 for (i = 0; i < tam ; i++) {
 l.adicionaNoInicio(i);
 }
 ASSERT_ANY_THROW(l.adicionaNoInicio(i+1));
 }*/

TEST_F(TesteLista, ListaAdicionaPosicao)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaNaPosicao( i, i );
    }
    ASSERT_EQ( l.posicao( i - 1 ), i - 1 );
}

TEST_F(TesteLista, ListaAdicionaPosicaoExcep1)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaNaPosicao( i, i );
    }
    ASSERT_ANY_THROW( l.adicionaNaPosicao( i + 1, i + 1 ) );
}

TEST_F(TesteLista, ListaAdicionaPosicaoExcep2)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaNaPosicao( i, i );
    }
    ASSERT_ANY_THROW( l.adicionaNaPosicao( i + 1, i + 2 ) );
}

TEST_F(TesteLista, ListaAdicionaOrdem)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaEmOrdem( tam - i );
    }
    ASSERT_TRUE( ( l.posicao( 1 ) < l.posicao( 2 ) ) );
}

/*TEST_F(TesteLista, ListaAdicionaOrdemExcep) {
 int i;
 for (i = 0; i < tam ; i++) {
 l.adicionaEmOrdem(tam-i);
 }
 ASSERT_ANY_THROW(l.adicionaEmOrdem(tam+2));
 }*/

TEST_F(TesteLista, ListaRemove)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_EQ( l.retira(), i - 1 );
}

TEST_F(TesteLista, ListaRemoveExcep)
{
    ASSERT_ANY_THROW( l.retira() );
}

TEST_F(TesteLista, ListaRemoveInicio)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_EQ( l.retiraDoInicio(), 0 );
}

TEST_F(TesteLista, ListaRemoveInicioExcep)
{
    ASSERT_ANY_THROW( l.retiraDoInicio() );
}

TEST_F(TesteLista, ListaRemovePosicao)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_EQ( l.retiraDaPosicao( 1 ), 1 );
}

TEST_F(TesteLista, ListaRemovePosicaoExcep1)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_ANY_THROW( l.retiraDaPosicao(tam+2) );
}

TEST_F(TesteLista, ListaRemovePosicaoExcep2)
{
    ASSERT_ANY_THROW( l.retiraDaPosicao( 1 ) );
}

TEST_F(TesteLista, ListaRemoveEspecifico)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    ASSERT_EQ( l.retiraEspecifico( 3 ), 3 );
}

TEST_F(TesteLista, ListaRemoveEspecificoExcep1)
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adiciona( i );
    }
    l.retiraEspecifico( 3 );
    ASSERT_ANY_THROW( l.retiraEspecifico( 3 ) );
}

TEST_F(TesteLista, ListaRemoveEspecificoExcep2)
{
    ASSERT_ANY_THROW( l.retiraEspecifico( 3 ) );
}

TEST_F(TesteLista, Maior)
{
    ASSERT_TRUE( l.maior( 2, 1 ) );
}

TEST_F(TesteLista, MaiorNeg)
{
    ASSERT_FALSE( l.maior( 1, 2 ) );
}

TEST_F(TesteLista, Menor)
{
    ASSERT_TRUE( l.menor( 1, 2 ) );
}

TEST_F(TesteLista, MenorNeg)
{
    ASSERT_FALSE( l.menor( 2, 1 ) );
}

TEST_F(TesteLista, Igual)
{
    ASSERT_TRUE( l.igual( 1, 1 ) );
}

TEST_F(TesteLista, IgualNeg)
{
    ASSERT_FALSE( l.igual( 2, 1 ) );
}
