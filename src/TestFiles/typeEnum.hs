{-description-}
data PeriodEnum =
    {-Day-}
    D
    {-Month-}
    | M
    {-Year-}
    | Y

instance Show PeriodEnum where
    show D = "day"
    show M = "month"
    show Y = "year"