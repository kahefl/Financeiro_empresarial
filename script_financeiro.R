library(dplyr)
library(formattable)
library(GetDFPData)
library(ggplot2)
library(ggthemes)

setwd("C:\\Users\\kahel\\OneDrive\\Documents\\Coisas do R\\scripts\\analise_acoes\\dados")

empresas = gdfpd.get.info.companies()

company = empresas %>% 
  filter(tickers == "PETR3;PETR4") # tick da empresa a ser pesquisada

di = "2000-01-01" # data inicial

df = "2020-12-31" # data final

infos_empresa= gdfpd.GetDFPData(name.companies = paste(company[1,1]), first.date = di, last.date = df)

# lucro da empresa

lucro_empresa = data.frame(infos_empresa$fr.income) %>% 
  select(ref.date, acc.desc, acc.value) %>% 
  mutate(valores = currency(c(acc.value*1000),symbol = "R$", digits = 0,  sep = " ", big.mark=".")) %>% 
  filter(acc.desc == "Lucro/Prejuízo do Período") %>% 
  rename(anos = "ref.date") %>% 
  mutate(cor = as.factor(ifelse(valores <= -0, yes = 1, no = 0)))

ggplot(lucro_empresa, aes(x = anos, y = valores, fill = cor)) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) +
  labs(x = " ", y = " ", title = paste(company[1,1]),
       subtitle = "Valores em reais") + theme_hc() +
  geom_text(aes(y = valores, label = valores),
            size = 3.5, vjust = 1.6, fontface= 2) +
  scale_x_date(breaks = lucro_empresa$anos,date_labels="%Y") +
  scale_fill_manual(values = c("#00BFFF","#FF2400")) +
  theme(axis.text.x.bottom = element_text(size = 11.5, colour = "black"),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.line.x = element_line(colour = "black", size = 2),
        axis.ticks.x.bottom = element_blank(),
        panel.grid.minor.x = element_line(colour = "grey50"),
        plot.subtitle=element_text(size = 9, hjust = 0.955))

ggsave("lucro_empresa.jpg", width = 14.5, height = 6)

################################################################################

# tipos de papeis ofertado 

prop_papeis = data.frame(infos_empresa$current.stock.composition)

prop_papeis = prop_papeis[1:2,3:4] %>% 
  mutate(prop = (number.of.stocks/sum(number.of.stocks)*100))

ggplot(prop_papeis, aes("", prop, fill = type.stock)) +
  geom_bar(width = 2, size = 1, color = NA, stat = "identity") +
  coord_polar("y") + theme_bw() +
  geom_text(aes(label = paste0(round(prop), "%")), 
            position = position_stack(vjust = 0.5), size = 5) +
  labs(x = NULL, y = NULL, fill = NULL, title = "", caption = " ") +
  guides(fill = guide_legend(reverse = TRUE))+ theme_void()+
  scale_fill_manual(values = c("#0047AB", "#F400A1")) +
  theme(legend.direction = "horizontal", legend.position = "bottom")

ggsave("papeis_empresa.jpg", width = 14.5, height = 6)

################################################################################

# receita da empresa

receita_empresa = data.frame(infos_empresa$fr.income.consolidated) %>% 
  select(ref.date, acc.desc, acc.value) %>% 
  mutate(valores = currency(c(acc.value*1000),symbol = "R$", digits = 0,  sep = " ", big.mark=".")) %>% 
  filter(acc.desc == "Receita de Venda de Bens e/ou Serviços") %>% 
  rename(anos = "ref.date") %>% 
  mutate(cor = as.factor(ifelse(valores <= -0, yes = 1, no = 0)))

ggplot(receita_empresa, aes(x = anos, y = valores, fill = cor)) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) +
  labs(x = " ", y = " ", title = paste(company[1,1]),
       subtitle = "Valores em reais") + theme_hc() +
  geom_text(aes(y = valores, label = valores), angle = 90,
            size = 3.5, vjust = 0.5, hjust = 1.5, fontface= 2) +
  scale_x_date(breaks = lucro_empresa$anos,date_labels="%Y") +
  scale_fill_manual(values = c("#00BFFF","#FF2400")) +
  theme(axis.text.x.bottom = element_text(size = 11.5, colour = "black"),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.line.x = element_line(colour = "black", size = 2),
        axis.ticks.x.bottom = element_blank(),
        panel.grid.minor.x = element_line(colour = "grey50"),
        plot.subtitle=element_text(size = 9, hjust = 0.955))

ggsave("receita_empresa.jpg", width = 14.5, height = 6)

################################################################################

dividendos = data.frame(infos_empresa$history.dividends)
