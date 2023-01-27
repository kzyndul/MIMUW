#ifndef IMAGES_H
#define IMAGES_H

#include <functional>
#include <cmath>
#include "coordinate.h"
#include "color.h"

using Fraction = double;

template<typename T>
using Base_image = std::function<T(Point const)>;


using Region = Base_image<bool>;


using Image = Base_image<Color>;

using Blend = Base_image<Fraction>;

template<typename T>
Base_image<T> constant(T t)
{
    return [=](Point const point)
    { return t; };
}

template<typename T>
Base_image<T> rotate(Base_image<T> image, double phi)
{
    return [=](Point const point)
    {
        Point temp = point.is_polar ? point : to_polar(point);
        Point help(temp.first, temp.second - phi, temp.is_polar);
        return std::invoke(image, from_polar(help));
    };
}

template<typename T>
Base_image<T> translate(Base_image<T> image, Vector v)
{
    return [=](Point const point)
    {
        Point temp = Point(point.first - v.first, point.second - v.second, false);
        return std::invoke(image, temp);
    };
}

template<typename T>
Base_image<T> scale(Base_image<T> image, double s)
{
    return [=](Point const point)
    {
        Point temp = Point(point.first / s, point.second / s, false);
        return std::invoke(image, temp);
    };
}

template<typename T>
Base_image<T> circle(Point q, double r, T inner, T outer)
{
    return [=](Point const point)
    {
        Point temp = !point.is_polar ? point : from_polar(point);
        Point tempq = !q.is_polar ? q : from_polar(q);

        return distance(tempq, temp) <= r ? inner : outer;
    };
}

template<typename T>
Base_image<T> checker(double d, T this_way, T that_way)
{
    return [=](Point const point)
    {

        return static_cast<int>(std::floor((point.first / d)) + std::floor((point.second / d))) % 2 == 0 ? this_way
                                                                                                         : that_way;
    };
}

template<typename T>
Base_image<T> polar_checker(double d, int n, T this_way, T that_way)
{
    return [=](Point const point)
    {
        Point temp = point.is_polar ? point : to_polar(point);
        return static_cast<int>(std::floor(((temp.second * n / 2) / (M_PI)))) % 2 == 0 ?
        checker(d, this_way, that_way)(temp) : checker(d, that_way, this_way)(temp);

    };
}

template<typename T>
Base_image<T> rings(Point q, double d, T this_way, T that_way)
{
    return [=](Point const point)
    {
        Point temp = !point.is_polar ? point : from_polar(point);
        Point tempq = !q.is_polar ? q : from_polar(q);
        return static_cast<int>(distance(tempq, temp) / d) % 2 == 0 ? this_way : that_way;
    };
}

template<typename T>
Base_image<T> vertical_stripe(double d, T this_way, T that_way)
{
    return [=](Point const point)
    {
        return std::abs(point.first) <= d / 2 ? this_way : that_way;
    };
}

inline Image cond(const Region &region, const Image &this_way, const Image &that_way)
{
    return [=](Point const point)
    { return std::invoke(region, point) ? std::invoke(this_way, point) : std::invoke(that_way, point); };
}

inline Image lerp(const Blend &blend, const Image &this_way, const Image &that_way)
{
    return [=](Point const point)
    {
        const double w = std::invoke(blend, point);
        const Color help1 = std::invoke(this_way, point);
        const Color help2 = std::invoke(that_way, point);
        const Color temp1 = std::invoke(&Color::weighted_mean, help1, help2, w);
        return temp1;
    };
}

inline Image darken(Image image, Blend blend)
{
    return [=](Point const point)
    {
        const double w = std::invoke(blend, point);
        const Color help = std::invoke(image, point);
        return std::invoke(&Color::weighted_mean, help, Colors::black, w);
    };
}

inline Image lighten(Image image, Blend blend)
{
    return [=](Point const point)
    {
        const double w = std::invoke(blend, point);
        const Color help = std::invoke(image, point);
        return std::invoke(&Color::weighted_mean, help, Colors::white, w);
    };
}

#endif
