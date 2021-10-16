{-description-}
data Period = MakePeriod {
    {-A time period multiplier, e.g. 1, 2 or 3 etc. A negative value can be used when specifying an offset relative to another date, e.g. -2 days.-}
    periodMultiplier :: int
    {-A time period, e.g. a day, week, month or year of the stream. If the periodMultiplier values is 0 (zero) then period must contain the value D (day).-}
    period :: periodEnum
    {-Test many-}
    testMany :: [testType]
    {-Test some-}
    testSome :: [testSomeType]
    {-Test zero or one-}
    testMaybeOne :: Maybe testZeroOneType
}
