{-The enumerated values to specified the period, e.g. day, week.-}
data EnumWithoutDisplay =
    {-Day-}
    D
    {-Month-}
    | M
    {-Year-}
    | Y

instance Show EnumWithoutDisplay where
    show D = "D"
    show M = "M"
    show Y = "Y"
