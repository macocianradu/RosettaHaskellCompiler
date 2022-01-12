data EnumWithoutDescription =
    X
    | Y

instance Show EnumWithoutDescription where
    show X = "xs"
    show Y = "ys"
