
#pragma once

template< typename > struct Traits
{
    static const bool enabled = true;
};


template<> struct Traits< AbstractionInterface >
{
    static constexpr bool isRemote = false;
};

