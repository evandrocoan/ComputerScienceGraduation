
#pragma once

template< bool condition, typename Then, typename Else >struct IF
{
    typedef Then Result;
};


template< typename Then, typename Else > struct IF< false, Then, Else >
{
    typedef Else Result;
};
