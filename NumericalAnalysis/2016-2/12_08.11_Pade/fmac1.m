function a = fmac1(n)
    a(1) = log(1.5);
    for i = 2 : n+1
        a(i) = (-1)^i / ((i-1)*3^(i-1));
    end
end