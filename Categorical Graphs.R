library(ggplot2)
library(grid)
library(gridExtra)

# Data Preparation
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data'
credit <- read.table(url)
colnames(credit) <- c("chk_acct", "duration", "credit_his", "purpose", "amount", "saving_acct",
                      "present_emp", "installment_rate", "sex", "other_debtor", "present_resid",
                      "property", "age", "other_install", "housing", "n_credits", "job", "n_people",
                      "telephone", "foreign", "response")
credit$response <- credit$response - 1
credit$response <- as.factor(credit$response)

levels(credit$chk_acct) <- c('<0', '0-200', '>=200', 'no account')
levels(credit$credit_his) <- c('none', 'paid all', 'paid existing', 'paid delay', 'critical')
levels(credit$present_emp) <- c('unemployed', '<1 year', '1-4 years', '4-7 years', '>7 years')
levels(credit$sex) <- c('M-Divorced', 'F-Divorced/Married', 'M-Single', 'M-Married')

# Function to create frequency tables and label positions
freq.fun <- function(cat.var)
{
  k <- as.data.frame(table(cat.var, credit$response),nrow=length(levels(cat.var)))
  names(k)[2] <- 'Default'
  k <- ddply(k, .(cat.var), transform, percent = Freq/sum(Freq) * 100)
  k <- ddply(k, .(cat.var), transform, pos = (cumsum(Freq) - 0.5 * Freq))
  k$label <- paste0(sprintf("%.0f", k$percent), "%")
  k$label <- ifelse(k$Freq<25 | k$percent<5, "", k$label) #hard-coded values, change as required
  return(k)
}

# Function to arrange ggplots in a grid with one legend
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

# Creating the plots
p1 <- ggplot(freq.fun(credit$chk_acct), aes(x = cat.var, y = Freq, fill = Default)) +
  geom_bar(stat = "identity") + xlab(NULL) + ylab(NULL) + coord_flip() +
  ggtitle('Checking Account') + theme(plot.title=element_text(size=10))+
  geom_text(aes(y = pos, label = label), size=2)

p2 <- ggplot(freq.fun(credit$credit_his), aes(x = cat.var, y = Freq, fill = Default)) +
  geom_bar(stat = "identity") + xlab(NULL) + ylab(NULL) + coord_flip() +
  ggtitle('Credit History') + theme(plot.title=element_text(size=10))+
  geom_text(aes(y = pos, label = label), size=2)

p3 <- ggplot(freq.fun(credit$present_emp), aes(x = cat.var, y = Freq, fill = Default)) + 
  geom_bar(stat = "identity") + xlab(NULL) + ylab(NULL) + coord_flip() +
  ggtitle('Purpose') + theme(plot.title=element_text(size=10))+
  geom_text(aes(y = pos, label = label), size=2)

p4 <- ggplot(freq.fun(credit$sex), aes(x = cat.var, y = Freq, fill = Default))+
  geom_bar(stat = "identity") + xlab(NULL) + ylab(NULL) + coord_flip() +
  ggtitle('Saving Account') + theme(plot.title=element_text(size=10))+
  geom_text(aes(y = pos, label = label), size=2)

grid_arrange_shared_legend(p1, p2, p3, p4)
