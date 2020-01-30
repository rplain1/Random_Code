

home_loan <- function(house, down_payment, ir, payment) {
  mortgage <- matrix(c(house*(1-down_payment), 
                       house*(1-down_payment)*(ir/12),
                       house*(1-down_payment)*(1+(ir/12)),
                       ((house*(1-down_payment))*(1+(ir/12))) - payment),
                     nrow = 1, ncol = 4)
  
  
  while (mortgage[nrow(mortgage),4] > payment) {
    var <- mortgage[nrow(mortgage), 4]
    rowN <- matrix(c(var, 
                     var*(ir/12),
                     var*(1+(ir/12)),
                     var*(1+(ir/12)) - payment),
                   nrow = 1, ncol = 4)
    print(rowN)
    mortgage <- rbind(mortgage, rowN)
  }
  print(nrow(mortgage)/12)
  return(data.frame(mortgage))
}

df <- home_loan(300000, .1, .04, 1990)
df$type <- '15 Year'

df30 <- home_loan(300000, .1, .04, 1289)
df30$type <- '30 Year'

df <- rbind(df, df30)
colnames(df) <- c('Principal','Interest','P+I','After.Payment','Type')

df %>%
  ggplot() +
  geom_line(aes(x=1:360, y=Interest, color=Type), data =. %>% 
                                      filter(Type=='30 Year')) +
  geom_line(aes(x=1:180, y=Interest, color=Type), data =. %>% 
              filter(Type=='15 Year')) 

df %>%
  ggplot() +
  geom_line(aes(x=1:360, y=Principal, color=Type),size=1.5, data =. %>% 
              filter(Type=='30 Year')) +
  geom_line(aes(x=1:180, y=Principal, color=Type),size=1.5, data =. %>% 
              filter(Type=='15 Year')) +
  geom_vline(xintercept = 60) +
  geom_point(aes(60, 199060.6)) +
  geom_point(aes(60, 244683.087)) +
  scale_y_continuous(labels= function(x) format(x, scientific = FALSE)) +
  labs(title = '30 Year Mortgage Pays $104,094 More in Total Interest',
       subtitle = 'After 5 Years, $50,000 more towards principal is paid \
30 Year Loan 10 Years later only $87,000 paid towards the principal after $154,680 in payments') +
  xlab('Months') +
  theme_minimal() +
  ggsave('Loan.png')
df %>%
  group_by(Type) %>%
  summarise(sum(Interest))
