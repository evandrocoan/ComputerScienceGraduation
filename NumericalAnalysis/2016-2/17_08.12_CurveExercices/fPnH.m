function y=fPnH(n,a,xi) 
     %Pn(xi)=a(1)+a(2)*xi+a(3)*xi^2+...+a(n)*xi^(n-1)+a(n+1)*xi^n 
     %Pn(xi)=a(1)+xi*(a(2)+xi*(a(3)+...+xi*(a(n)+xi*a(n+1))...))% HORNER 
    for ip=1:length(xi) %calcula y p/ cada elemento de 
        xi;
         y(ip)=a(n+1); 
         for i=n:-1:1 
             y(ip)=a(i)+y(ip)*xi(ip); 
         end 
    end 
end 