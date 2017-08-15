clear
clc
format long
split_long_rows(0)
#output_precision(30)
#output_max_field_width(0)


# Questão 2) equação do Enunciado, 
n_1 = 30
n_2 = 50
n   = n_2

# Para i = 1
t( 1, 1 ) = 0;
r( 1, 1 ) = 2;
d( 1, 1 ) = -1;
b( 1, 1 ) = -1;

# Para i = 2, ..., n_1 - 1
for i = 2 : n_1 - 1
    
    t( i, 1 ) = -1;
    r( i, 1 ) = 3;
    d( i, 1 ) = -1;
    b( i, 1 ) = 1;
    
end

# Para i = n_1, ..., n_2 - 1
for i = n_1 : n_2 - 1
    
    t( i, 1 ) = -1;
    r( i, 1 ) = 3;
    d( i, 1 ) = -1;
    b( i, 1 ) = 2;
    
end

# Para i = n_2
t( n_2, 1 ) = -1;
r( n_2, 1 ) = 1;
d( n_2, 1 ) = 0;
b( n_2, 1 ) = 3;

t;
r;
d;
b;


x = fGaussTrid( n, t, r, d, b )






