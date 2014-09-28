/*
 Copyright [2014] <Jean Martina>
 */
#include <cstdlib>

#include <gtest/gtest.h>
#include "ListaDuplaEnc.hpp"
#define tam 10
#define offset 1000

int main( int argc, char* argv[] )
{
    ::testing::InitGoogleTest( &argc, argv );
    return RUN_ALL_TESTS();
}

class Objeto
{
};

class TesteLista: public ::testing::Test
{
    protected:
        ListaDupla< int > l;
        ListaDupla< Objeto* > lobj;
};

TEST_F( TesteLista, ListaVazia )
{
    ASSERT_TRUE( l.listaVazia() );
    ASSERT_TRUE( lobj.listaVazia() );
}

TEST_F( TesteLista, ListaVaziaNeg )
{
    l.adicionaDuplo( 1 );
    ASSERT_FALSE( l.listaVazia() );
    lobj.adicionaDuplo( new Objeto );
    ASSERT_FALSE( lobj.listaVazia() );
}

TEST_F( TesteLista, ListaContem )
{
    Objeto* o = new Objeto();
    l.adicionaDuplo( 1 );
    ASSERT_TRUE( l.contemDuplo( 1 ) );
    lobj.adicionaDuplo( o );
    ASSERT_TRUE( lobj.contemDuplo( o ) );
}

TEST_F( TesteLista, ListaContemNeg )
{
    Objeto* o = new Objeto();
    Objeto* o2 = new Objeto();
    l.adicionaDuplo( 1 );
    ASSERT_FALSE( l.contemDuplo( 2 ) );
    lobj.adicionaDuplo( o );
    ASSERT_FALSE( lobj.contemDuplo( o2 ) );
}

TEST_F( TesteLista, ListaDestroi )
{
    l.adicionaDuplo( 1 );
    l.destroiListaDuplo();
    ASSERT_TRUE( l.listaVazia() );
}

TEST_F( TesteLista, ListaPosicao )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_EQ( l.posicaoDuplo( 5 ), 5 );
}

TEST_F( TesteLista, ListaPosicaoExcep )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_ANY_THROW( l.posicaoDuplo( tam + 2 ) );
}

/*TEST_F(TesteLista, ListaAdiciona) {
 int i;
 for (i = 0; i < tam ; i++) {
 l.adiciona(i);
 }
 ASSERT_ANY_THROW(l.adiciona(i+1));
 }*/

TEST_F( TesteLista, ListaAdicionaInicio )
{
    int i;
    for( i = 0; i < tam - 1; i++ )
    {
        l.adicionaNoInicioDuplo( i );
    }
    l.adicionaNoInicioDuplo( i + 1 );
    ASSERT_EQ( l.posicaoDuplo( i + 1 ), 0 );
}

TEST_F( TesteLista, ListaAdicionaPosicao )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaNaPosicaoDuplo( i, i );
    }
    ASSERT_EQ( l.posicaoDuplo( i - 1 ), i - 1 );
}

TEST_F( TesteLista, ListaAdicionaPosicaoExcep1 )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaNaPosicaoDuplo( i, i );
    }
    ASSERT_ANY_THROW( l.adicionaNaPosicaoDuplo( i + 1, i + 1 ) );
}

TEST_F( TesteLista, ListaAdicionaPosicaoExcep2 )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaNaPosicaoDuplo( i, i );
    }
    ASSERT_ANY_THROW( l.adicionaNaPosicaoDuplo( i + 1, i + 2 ) );
}

TEST_F( TesteLista, ListaAdicionaOrdem )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaEmOrdem( tam - i );
    }
    ASSERT_TRUE( ( l.posicaoDuplo( 1 ) < l.posicaoDuplo( 2 ) ) );
}

TEST_F( TesteLista, ListaRemove )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_EQ( l.retiraDuplo(), i - 1 );
}

TEST_F( TesteLista, ListaRemoveExcep )
{
    ASSERT_ANY_THROW( l.retiraDuplo() );
}

TEST_F( TesteLista, ListaRemoveInicio )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_EQ( l.retiraDoInicioDuplo(), 0 );
}

TEST_F( TesteLista, ListaRemoveInicioExcep )
{
    ASSERT_ANY_THROW( l.retiraDoInicioDuplo() );
}

TEST_F( TesteLista, ListaRemovePosicao )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_EQ( l.retiraDaPosicaoDuplo( 1 ), 1 );
}

TEST_F( TesteLista, ListaRemovePosicaoExcep1 )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_ANY_THROW( l.retiraDaPosicaoDuplo( tam + 2 ) );
}

TEST_F( TesteLista, ListaRemovePosicaoExcep2 )
{
    ASSERT_ANY_THROW( l.retiraDaPosicaoDuplo( 1 ) );
}

TEST_F( TesteLista, ListaRemoveEspecifico )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    ASSERT_EQ( l.retiraEspecificoDuplo( 3 ), 3 );
}

TEST_F( TesteLista, ListaRemoveEspecificoExcep1 )
{
    int i;
    for( i = 0; i < tam; i++ )
    {
        l.adicionaDuplo( i );
    }
    l.retiraEspecificoDuplo( 3 );
    ASSERT_ANY_THROW( l.retiraEspecificoDuplo( 3 ) );
}

TEST_F( TesteLista, ListaRemoveEspecificoExcep2 )
{
    ASSERT_ANY_THROW( l.retiraEspecificoDuplo( 3 ) );
}

TEST_F( TesteLista, Maior )
{
    ASSERT_TRUE( l.maior( 2, 1 ) );
}

TEST_F( TesteLista, MaiorNeg )
{
    ASSERT_FALSE( l.maior( 1, 2 ) );
}

TEST_F( TesteLista, Menor )
{
    ASSERT_TRUE( l.menor( 1, 2 ) );
}

TEST_F( TesteLista, MenorNeg )
{
    ASSERT_FALSE( l.menor( 2, 1 ) );
}

TEST_F( TesteLista, Igual )
{
    ASSERT_TRUE( l.igual( 1, 1 ) );
}

TEST_F( TesteLista, IgualNeg )
{
    ASSERT_FALSE( l.igual( 2, 1 ) );
}
