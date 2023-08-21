#ifndef FUNCTIONAL_H
#define FUNCTIONAL_H

#include <functional>

inline auto compose()
{
    return std::identity();
}

template <typename T, typename... Args>
auto compose(T f, Args... args)
{
    return [=](auto x) { return compose(args...)(f(x)); };
}



template<typename H, typename...  Fs>
auto lift(H h, Fs... fs)
{
    return std::bind(h, fs(std::placeholders::_1)...);
}

#endif