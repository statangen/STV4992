library(irr)

round1 <- read_excel("~/Desktop/ICRT.xlsx", 1)
round2 <- read_excel("~/Desktop/ICRT.xlsx", 2)


icrt <- data.frame(
  r1 = round1$NORCAP,
  r2 = round2$NORCAP
)


result <- kripp.alpha(t(as.matrix(icrt)), method = "nominal")
print(result$value)
