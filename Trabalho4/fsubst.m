function x=fsubst(n,A)

	if abs(A(n,n))<1e-14 && abs(A(n,n+1))<1e-14

		printf("\n sistema indeterminado \n")
		x(n)=0;
	end
	if abs(A(n,n))<1e-14 && abs(A(n,n+1))>1e-14

			printf("\n sistema impossivel \n")
			x(n)= nan
	end
	if abs(A(n,n))>1e-14 

			x(n)=A(n,n+1)/A(n,n);

  			for i=n-1:-1:1

				soma=0;

				for j=i+1:n

					soma = soma + A(i,j)*x(j);

				endfor    
 
				x(i)=(A(i,n+1) - soma)/A(i,i);

			endfor

	endif

endfunction
