#include "gtest/gtest.h"

template <typename H, typename T>
struct typelist
{
    typedef H head;
    typedef T tail;
};

struct null_typelist {};

template <class TList> struct Length;
template <> struct Length<null_typelist>
{
    enum { value = 0 };
};

template <class T, class U>
struct Length< typelist<T, U> >
{
    enum { value = 1 + Length<U>::value };
};

typedef typelist<int,
        typelist<float,
        null_typelist > > testList;

template<int N>
struct IntWrapper {};

void testLength()
{
    ::testing::StaticAssertTypeEq<
        IntWrapper<2>,
        IntWrapper<Length<testList>::value> >();
}
