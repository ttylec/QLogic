-- State:

data Value = Probability Double | Variable

data State q a = State q (a -> Value)
