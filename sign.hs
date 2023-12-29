-- Return "Negetive", "Zero" or "Positive" for a given number
sign :: Int -> String
sign n
    |    n<0 = "Negetive"
    |    n==0 = "Zero"
    |    n>0 = "Positive"