library(ggplot2)

sale_price <- seq(0, 15e6, 100000)
# 2) The rate of the tax shall be one-half of one percent (0.5%) (five dollars
# ($5.00) for each one thousand dollars ($1,000.00) or fractional part thereof)
# for the value of consideration for property paid in excess of one million
# eight-hundred thousand dollars $1,800,000 but less than two million
# five-hundred thousand dollars($2,500,000.00).
tier2_fn <- function(x) {
  0.005 * min(2.5e6-1.8e6, max(0, x - 1.8e6))
}
tier2 <- sapply(sale_price, tier2_fn)

# (3) The rate of the tax shall be one percent (1%) (ten dollars ($10.00) for
# each one thousand dollars ($1,000.00)or fractional part thereof) for the
# value of the consideration for property conveyed in excess of two million
# five-hundred thousand dollars ($2,500,000) but less than three million
# five-hundred thousand dollars ($3,500,000).
tier3_fn <- function(x) {
  0.01 * min(3.5e6-2.5e6, max(0, x - 2.5e6))
}
tier3 <- sapply(sale_price, tier3_fn)


# (4) The rate of the tax shall be one and one-half percent (1.50%) (fifteen
# dollars ($15.00) for each one thousand dollars ($1,000.00) or fractional part
# thereof) for the value of the consideration for property conveyed in excess
# of three million five-hundred dollars ($3,500,000) but less than four million
# five-hundred thousand dollars($4,500,000).
tier4_fn <- function(x) {
  0.015 * min(4.5e6-3.5e6, max(0, x - 3.5e6))
}
tier4 <- sapply(sale_price, tier4_fn)
                
# (5) The rate of the tax shall be two percent (2.00%) (twenty dollars ($20.00)
# for each one thousand dollars ($1,000.00) or fractional part thereof) for the
# value of the consideration for property conveyed in excess of four million
# five-hundred thousand dollars ($4,500,000) provided, however, that the
# maximum amount of tax paid on any taxable transaction shall not exceed two
# hundred thousand dollars ($200,000).
cap_value <- 2e5 - max(tier2) - max(tier3) - max(tier4)
tier5_fn <- function(x) {
  min(cap_value, 0.02 * max(0, x - 4.5e6))
}
tier5 <- sapply(sale_price, tier5_fn)
                
df <- data.frame(
  sale_price = rep(sale_price,4),
  tier = c(rep("tier2", length(tier2)), rep("tier3", length(tier3)), rep("tier4", length(tier4)), rep("tier5", length(tier5))),
  transfer_tax = c(tier2, tier3, tier4, tier5)
)
df$tier <- factor(df$tier, levels=c("tier5", "tier4", "tier3", "tier2"))
x_ticks <- c(0, 1.8e6, 2.5e6, 3.5e6, 4.5e6, 6e6, 8e6, 10e6, 12e6, 13.1e6, 15e6)
index_of_6e6 <- match(6e6, sale_price)
m <- match(x_ticks, sale_price)
y_ticks <- sapply(m, function(x) tier2[x] + tier3[x] + tier4[x] + tier5[x])
# remove the repeated '0' entry
y_ticks <- unique(y_ticks)

# Create the stacked barplot with a numeric x-axis
p <- ggplot(df, aes(x = sale_price, y = transfer_tax, fill=tier)) +
  xlab("Sale Price") + ylab("Transfer Tax") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", position="stack") +
  scale_x_continuous(labels = function(x) sprintf("$%1.1fM", x / 1e6), breaks=x_ticks, minor_breaks=c(0, 15e6)) +
  scale_y_continuous(labels = function(y) sprintf("$%1.1fk", y / 1e3), breaks=y_ticks, minor_breaks=c(0, 200000)) +
  scale_fill_hue(name="", labels=c("$4.5M+", "$3.5M-4.5M", "$2.5M-3.5M", "$1.8M-2.5M"))

print(p)
