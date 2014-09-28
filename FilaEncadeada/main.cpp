/* Copyright [2014] <Jean Martina>
 * TesteFilaEnc.cpp
 */

#include <stdio.h>
#include <gtest/gtest.h>
#include "FilaEnc.hpp"


int main(int argc, char* argv[]) {
    ::testing::InitGoogleTest(&argc, argv);
    int a = RUN_ALL_TESTS();
    return a;
}

class Objeto{
};

class TesteFila: public ::testing::Test{
 protected:
    FilaEnc<int> p;
    FilaEnc<Objeto> pobj;;
};

TEST_F(TesteFila, FilaVazia) {
    ASSERT_TRUE(p.filaVazia());
}


TEST_F(TesteFila, InsereElemento) {
    p.inclui(0);
    ASSERT_EQ(0, p.ultimo());
}

TEST_F(TesteFila, Retira) {
    p.inclui(0);
    p.inclui(1);
    p.inclui(2);
    ASSERT_EQ(0, p.retira());
    ASSERT_EQ(2, p.ultimo());
}

TEST_F(TesteFila, Ultimo) {
    p.inclui(0);
    p.inclui(1);
    ASSERT_EQ(1, p.ultimo());
}

TEST_F(TesteFila, PosicaoUltimo) {
    p.inclui(0);
    p.inclui(1);
    p.inclui(2);
    ASSERT_EQ(2, p.ultimo());
}

TEST_F(TesteFila, ExcecaoFilaVazia) {
    EXPECT_ANY_THROW(p.retira());
}

TEST_F(TesteFila, LimpaFila) {
    p.inclui(0);
    p.inclui(1);
    p.inclui(2);
    p.inclui(3);
    p.inclui(4);
    p.inclui(5);
    p.limparFila();
    EXPECT_ANY_THROW(p.ultimo());
}
