color<-sample(colors()[!grepl("gre|ay",colors())],5)
print(color)
par(mfrow=c(2,2))

barplot(quantile(DF1$RBCT2,probs=seq(.75,1,length.out = 20)),
        col=sample(colors()[!grepl("gre|ay",colors())],5))
barplot(quantile(DF1$RBCT2,probs=seq(.99,1,length.out = 20),
                 col=sample(colors()[!grepl("gre|ay",colors())],5)))
barplot(quantile(DF1$RBCT2,probs=seq(.999,1,length.out = 20),
                 col=sample(colors()[!grepl("gre|ay",colors())],5)))
plot(quantile(DF1$RBCT2,probs=seq(0,1,.01)),
        col=sample(colors()[!grepl("gre|ay",colors())],1),type="l")
# lines(quantile(DF1$rbc1rwaj,probs=seq(0,1,.01),na.rm = T),
#       col=sample(colors()[!grepl("gre|ay",colors())],1),type="l")
# lines(quantile(DF1$rbcrwaj,probs=seq(0,1,.01),na.rm = T),
#       col=sample(colors()[!grepl("gre|ay",colors())],1),type="l")
lines(quantile(DF1$asset,probs=seq(0,1,.01),na.rm = T),
      col=sample(colors()[!grepl("gre|ay",colors())],1),type="l")
