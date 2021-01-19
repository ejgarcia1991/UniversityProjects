listaGRain <-list(~A, ~S, ~E | A : S, ~O | E, ~R | E, ~T | O : R) 
dagGRain<-dagList(listaGRain)

A.st <- c("young", "adult", "old")
S.st <- c("M", "F")
E.st <- c("high", "uni")
O.st <- c("emp", "self")
R.st <- c("small", "big")
T.st <- c("car", "train", "other")

A.CPT <- cptable(~A, values=c(3, 5, 2), levels=A.st)
S.CPT <- cptable(~S, values=c(6, 4), levels=S.st)
O.CPT <- cptable(~O+E, values=c(96, 4, 92, 8), levels=O.st)
R.CPT <- cptable(~R+E, values=c(25, 75, 20, 80), levels=R.st)
E.CPT <- cptable(~E+A+S, values=c(75, 25, 72, 28, 88, 12, 64, 36, 70, 30, 90, 10), levels=E.st)
T.CPT <- cptable(~T+O+R, values=c(48, 42, 10, 56, 36, 8, 58, 24, 18, 70, 21, 9), levels=T.st)

potentialList <- compileCPT(list(A.CPT, S.CPT, O.CPT, R.CPT, E.CPT, T.CPT ))

redGrain <- grain(potentialList)
junctionTreeRedGrain <- compile(redGrain)