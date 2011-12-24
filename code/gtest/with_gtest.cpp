...
class ClosureTest : public ::testing::Test
{
public:
    ClosureTestCallee* fpTestCallee;
    virtual void SetUp()
    {
        fpTestCallee = new ClosureTestCallee();
    }
    virtual void TearDown()
    {
        delete fpTestCallee;
    }
};

TEST_F(ClosureTest, closureWithSingleParameter) {
    ...
    EXPECT_EQ(fExpectedCalls, fCallbackCalled);
}
...

