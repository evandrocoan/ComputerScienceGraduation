/* Copyright [2014] <Jean Martina>
 * TestePilhaEnc.cpp
 */

#include <stdio.h>
#include <gtest/gtest.h>
#include "PilhaEnc.hpp"

int main( int argc, char* argv[] )
{
    ::testing::InitGoogleTest( &argc, argv );
    int a = RUN_ALL_TESTS();
    return a;
}

class Objeto
{
};

class TestePilha: public ::testing::Test
{
    protected:
        PilhaEnc< int > p;
        PilhaEnc< Objeto > pobj;
};

TEST_F(TestePilha, PilhaVazia)
{
    ASSERT_TRUE( p.PilhaVazia() );
}

TEST_F(TestePilha, InsereElemento)
{
    p.empilha( 0 );
    ASSERT_EQ( 0, p.topo() );
}

TEST_F(TestePilha, Desempilha)
{
    p.empilha( 0 );
    p.empilha( 1 );
    p.desempilha();
    ASSERT_EQ( 0, p.topo() );
}

TEST_F(TestePilha, Topo)
{
    p.empilha( 0 );
    p.empilha( 1 );
    ASSERT_EQ( 1, p.topo() );
}

TEST_F(TestePilha, PosicaoTopo)
{
    p.empilha( 0 );
    p.empilha( 1 );
    p.empilha( 2 );
    ASSERT_EQ( 2, p.topo() );
}

TEST_F(TestePilha, ExcecaoPilhaVazia)
{
    EXPECT_ANY_THROW( p.desempilha() );
}

TEST_F(TestePilha, LimpaPilha)
{
    p.empilha( 0 );
    p.empilha( 1 );
    p.empilha( 2 );
    p.empilha( 3 );
    p.empilha( 4 );
    p.empilha( 5 );
    p.limparPilha();
    EXPECT_ANY_THROW( p.topo() );
}
