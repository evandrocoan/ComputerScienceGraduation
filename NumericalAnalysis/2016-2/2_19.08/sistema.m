A = [
     0       e        sqrt(2)      1;
     sqrt(3) sqrt(1) -1/(sqrt(3))  2;
     10      2        sqrt(21)     113; 
    ]
#linhas quase iguais da SI    
n = size(A,1)
#auxiliar para zerar a l2   
#A(i,j) para todo j -> A(i,:)
#aux1 = A(2,1)/A(1,1);
#A(2,:) = A(2,:) - aux1*(A(1,:)); #l2 = l2 - aux l1
#aux2 = A(3,1)/A(1,1);
#A(3,:) = A(3,:) - aux2*(A(1,:));#l3 = l3 - aux2*l1
#aux3 = A(3,2)/A(2,2);
#A(3,:) = A(3,:) - aux3*(A(2,:)); #l3 = l3 - aux3*l2

#testar A(m,n)


x = fgauss(A,n)
residuo_max = rmax(A,n,x)