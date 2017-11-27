boomBangs xs = [if x < 10 then "Boom" else "Bang" | x <- xs, even x]

main = do
    print (boomBangs [1..20])