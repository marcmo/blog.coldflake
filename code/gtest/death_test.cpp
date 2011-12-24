#include "gtest/gtest.h"

namespace {

int foo(int x){
    assert(x < 5);
}

TEST(DeathTest, foo) {
  ASSERT_DEATH( foo(10); , ".*Assertion.*");
}


}  // namespace

