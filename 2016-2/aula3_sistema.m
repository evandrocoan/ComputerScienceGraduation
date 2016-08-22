A = [ pi,      e,        sqrt(2),    1; 
      sqrt(3), sqrt(10), -1/sqrt(3), 2; 
      e,       -sqrt(2), 1/sqrt(10), 3 ]

B = A;


aux1 = A(2,1)/A(1,1);
A(2,:)=A(2,:)-aux1*A(1,:);


aux2 = A(3,1)/A(1,1);
A(3,:)=A(3,:)-aux2*A(1,:);


aux3 = A(3,2)/A(2,2);
A(3,:)=A(3,:)-aux3*A(2,:);

A

X = aula3_fgauss( B )



