#Reading in data frames from Python
united_states <- read.csv("united_states.csv")["efficiency_score"]
united_kingdom <- read.csv("united_kingdom.csv")["efficiency_score"]
australia <- read.csv("australia.csv")["efficiency_score"]
brazil <- read.csv("brazil.csv")["efficiency_score"]
netherlands <- read.csv("netherlands.csv")["efficiency_score"]
spain <- read.csv("spain.csv")["efficiency_score"]
france <- read.csv("france.csv")["efficiency_score"]
italy <- read.csv("italy.csv")["efficiency_score"]
canada <- read.csv("canada.csv")["efficiency_score"]
germany <- read.csv("germany.csv")["efficiency_score"]

# Conducting wilcoxon and t test on efficiency scores
# (Germany is the only normally distributed data)
# Setting mu value to median value of that specific data frame
wilcox.test(united_states$efficiency_score,
            mu = median(united_states[["efficiency_score"]]))
wilcox.test(united_kingdom$efficiency_score,
            mu = median(united_kingdom[["efficiency_score"]]))
wilcox.test(australia$efficiency_score,
            mu = median(australia[["efficiency_score"]]))
wilcox.test(brazil$efficiency_score, mu = median(brazil[["efficiency_score"]]))
wilcox.test(netherlands$efficiency_score,
            mu = median(netherlands[["efficiency_score"]]))
wilcox.test(spain$efficiency_score, mu = median(spain[["efficiency_score"]]))
wilcox.test(france$efficiency_score, mu = median(france[["efficiency_score"]]))
wilcox.test(italy$efficiency_score, mu = median(italy[["efficiency_score"]]))
wilcox.test(canada$efficiency_score, mu = median(canada[["efficiency_score"]]))
wilcox.test(germany$efficiency_score,
            mu = median(germany[["efficiency_score"]]))
t.test(germany$efficiency_score, mu = median(germany[["efficiency_score"]]))

# Compaing efficiency values between Italy and Spain based on previous
# Wilcoxon test and averages
t.test(italy, spain, alternative = "two.sided", var.equal = FALSE)
t.test(italy, spain, alternative = "less", var.equal = FALSE)
t.test(italy, spain, alternative = "greater", var.equal = FALSE)