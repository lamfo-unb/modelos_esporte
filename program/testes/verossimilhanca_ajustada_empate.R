logDirichregregt <- function(parm){
  yr <- (yr*(nrow(yr) - 1) + 1/ncol(yr))/(nrow(yr))
  # parmcompleto <- rep(0,ncol(yr)*ncol(xr))
  # parmcompleto[-indzero] <- parm
  parmreg <- matrix(parm,nrow=ncol(yr),byrow = F)
  ai <- xr %*% t(parmreg)
  ai <- exp(ai)
  Ai <- apply(ai,1,sum)
  v0 <- ((ai-1)*log(yr)-log(gamma(ai)))
  v1 <- apply(v0,1,sum)
  veroi <- sum(log(gamma(Ai)) + v1)
  ## regularização ridge&lasso
  veroi <- veroi - k*(alpha_regula*sum((parm^2)) +
                         (1-alpha_regula)*sum(abs(parm)))
  #   sum((parm[indzero]^2)*10^5)
  return(veroi)
}


sigma <- .7
BASEGLM <- data.frame(cbind(yr[,2],exp(-sigma*xr[,3]^2)))
res <- glm(X1~X2,data =BASEGLM , family=binomial(link = "logit"));summary(res)
table(yr[,2],res$fitted.values>.5)

BASEGLM %>% group_by(X1) %>%
  summarise(M = mean(X2))
# 
# 
# 
# res <- glm(yr[,2]~exp(-sigma*(xr[,-1]^2)),family = binomial())
# table(yr[,2],res$fitted.values>.5)

