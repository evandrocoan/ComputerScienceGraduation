function Sn = fSn(n, a, b)
    if(mod(n,2) != 0)
        n+=1;
    end
    h = (b - a)/n;
    x = a : h : b;
    y = f(x);
    soma1 = 0;
    for i = 2 : 2 : n
        soma1 += y(i);
    endfor
    soma2 = 0;
    for i = 3 : 2 : n-1
        soma2 += y(i);
    endfor
    
    Sn = (h/3)*(y(1) + 4*soma1 + 2*soma2 + y(n+1));
endfunction