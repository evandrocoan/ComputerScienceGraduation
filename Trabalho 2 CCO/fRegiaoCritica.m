function bool = fRegiaoCritica( a, b, h )
    bool = 0;
    if( a < 1/h | b < 1/h )
        bool = 1;
end
