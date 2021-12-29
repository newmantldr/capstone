econ123cw <- read.csv("C:/Users/tyler/Downloads/fixedriotapi.csv", 
                      stringsAsFactors = FALSE)
games = econ123cw$wins + econ123cw$losses
econ123cw$games = games
winrate = abs(econ123cw$wins / econ123cw$games)
econ123cw$winrate = winrate
econ123cw$winrate = round(econ123cw$winrate, 3)

cleanedset = subset(econ123cw, summoner.level != -1, champion.mastery != -1)

tier.order = ordered(cleanedset$tier, levels = c('IRON', 'BRONZE', 'SILVER', 'GOLD', 'PLATINUM', 'DIAMOND'))

rank.order = ordered(cleanedset$rank, levels = c('IV', 'III', 'II', 'I'))

regression = lm(winrate ~ tier.order + rank.order + leaguePoints + summoner.level + games + champion.mastery, data = cleanedset)
summary_name = summary(regression)
summary_name$coefficients[, 2:4] <- lmtest::coeftest(regression, vcov = sandwich::vcovHC(regression, type="HC1"))[,2:4]
summary(regression)
summary(cleanedset)
barplot(table(cleanedset$summoner.level))

nosmurfs = cleanedset[cleanedset$summoner.level >= 50 & cleanedset$games >= 50,]
nosmurfstier.order = factor(nosmurfs$tier, levels = c('IRON', 'BRONZE', 'SILVER', 'GOLD', 'PLATINUM', 'DIAMOND'), ordered = TRUE)

nosmurfsrank.order = factor(nosmurfs$rank, levels = c('IV', 'III', 'II', 'I'), ordered = TRUE)
barplot(table(nosmurfstier.order))
regression_nosmurfs = lm(winrate ~ nosmurfstier.order + nosmurfsrank.order + leaguePoints + summoner.level + games + nosmurfstier.order*games 
                         + champion.mastery, data = nosmurfs)
summary_name = summary(regression)
#summary_name$coefficients[, 2:4] <- lmtest::coeftest(regression_nosmurfs, vcov = sandwich::vcovHC(regression, type="HC1"))[,2:4]
summary(regression_nosmurfs)
summary(nosmurfs)
summary(nosmurfstier.order)

pairs(~ nosmurfstier.order + nosmurfsrank.order + leaguePoints + summoner.level + games + nosmurfstier.order*games 
      + champion.mastery, data = nosmurfs)
pairs(winrate ~ nosmurfstier.order, data = nosmurfs)
pairs(winrate ~ nosmurfstier.order*games, data = nosmurfs)
pairs(winrate ~ nosmurfsrank.order, data = nosmurfs)
pairs(winrate ~ leaguePoints, data = nosmurfs)
pairs(winrate ~ summoner.level, data = nosmurfs)
pairs(winrate ~ summoner.level^2, data = nosmurfs)
pairs(winrate ~ games, data = nosmurfs)
pairs(winrate ~ champion.mastery, data = nosmurfs)
hist(nosmurfs$winrate)
abline(v=mean(nosmurfs$winrate), lwd = 3, col = "blue")
abline(v=median(nosmurfs$winrate), lwd = 3, col="red")
