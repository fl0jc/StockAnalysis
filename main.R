library(quantmod)
library(ggplot2)
ticker <- getSymbols("NVDA",
                   src = "yahoo",
                   from = "2018-01-01",
                   to = "2024-01-01",
                   auto.assign = FALSE)
head(ticker)
tail(ticker)
summary(ticker)
str(ticker)
ggplot(data = ticker, aes(x = index(ticker), y = ticker[,6])) +
  geom_line(color = "darkblue") +
  ggtitle("NVIDIA,prices series") +
  xlab("Date") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months")

ticker_ <- subset(ticker, index(ticker) >= "2018-01-01")

ticker_ma10 <- rollmean(ticker_[,6], 10, fill = list(NA, NULL, NA), align = "right")
ticker_ma30 <- rollmean(ticker_[,6], 30, fill = list(NA, NULL, NA), align = "right")

ticker_$ma10 <- coredata(ticker_ma10)
ticker_$ma30 <- coredata(ticker_ma30)

ggplot(data = ticker_, aes(x = index(ticker_))) +
  geom_line(aes(y = ticker_[,6], color = "NVIDIA")) +
  geom_line(aes(y = ticker_$ma10, color = "MM10")) +
  geom_line(aes(y = ticker_$ma30, color = "MM30")) +
  ggtitle("NVIDIA, prices series") +
  xlab("Date") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
  scale_colour_manual("Series", values=c("NVIDIA"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))

ticker_return <- diff(log(ticker[,6]))
ticker_return <- ticker_return[-1,]
summary(ticker_return)

ggplot(data = ticker_return, aes(x = index(ticker_return), y = ticker_return)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("NVIDIA, returns series") +
  xlab("Date") + ylab("Return") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months")
