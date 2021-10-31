{-description-}
data Period = MakePeriod {
    {-A time period multiplier, e.g. 1, 2 or 3 etc. A negative value can be used when specifying an offset relative to another date, e.g. -2 days.-}
    periodMultiplier :: int
    {-Test many-}
    testMany :: [TestType]
    {-Test some-}
    testSome :: [TestSomeType]
    {-Test zero or one-}
    testMaybeOne :: Maybe TestZeroOneType
}
