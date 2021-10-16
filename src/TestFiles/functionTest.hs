{-Function specification for the observation of an equity price, based on the attributes of the 'EquityValuation' class.-}
EquityPriceObservation :: Equity
                       -> AdjustableOrRelativeDate
                       -> Maybe BusinessCenterTime
                       -> Maybe TimeTypeEnum
                       -> [DeterminationMethodEnum]
                       -> ObservationPrimitive