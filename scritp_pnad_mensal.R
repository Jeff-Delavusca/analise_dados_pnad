# Pacotes necessários
library(tidyverse)
library(lubridate)
library(sidrar)
library(zoo)
library(scales)
library(timetk)
library(knitr)

# Carregando dados agregados PNAD

# Obtendo dados da população
populacao = get_sidra(api = '/t/6022/n1/all/v/606/p/all') %>%
  mutate(date = parse_date(`Trimestre Móvel (Código)`, format='%Y%m')) %>%
  select(date, Valor) %>%
  as_tibble()

# Visualizando os últimos registros da população
tail(populacao)

# Plotando gráfico da população ao longo do tempo
ggplot(populacao, aes(x=date, y=Valor))+
  geom_line()

# Obtendo dados sobre condição de emprego
names = c("date", "pnea", "pea", "desocupada", "ocupada", "pia")
condicao = get_sidra(api = '/t/6318/n1/all/v/1641/p/all/c629/all') %>%
  mutate(date = parse_date(`Trimestre Móvel (Código)`, format='%Y%m')) %>%
  select(date, "Condição em relação à força de trabalho e condição de ocupação", Valor) %>%
  spread("Condição em relação à força de trabalho e condição de ocupação", Valor) %>%
  `colnames<-`(names) %>%
  as_tibble()

# Juntando dados de população e condição de emprego
agregado_pnad = inner_join(populacao, condicao, by='date') %>%
  rename(populacao = Valor) %>%
  mutate(inativos = populacao - pia,
         desemprego = desocupada/pea*100,
         participacao = pea/pia*100) %>%
  select(date, populacao, inativos, pia, pea, pnea, ocupada, desocupada, desemprego, participacao)

# Visualizando os últimos registros do agregado PNAD
agregado_pnad %>% 
  select(-desemprego, -participacao) %>% 
  tail() %>% 
  kable()

# Visualizando os últimos registros do agregado PNAD (desemprego e participação)
agregado_pnad %>% 
  select(date, desemprego, participacao) %>% 
  tail() %>% 
  kable()

# Transformando os dados em formato longo para plotagem
agregado_pnad_long = 
  agregado_pnad %>% 
  gather(variavel, Valor, -date)

# Plotando gráfico de desemprego e participação
filter(agregado_pnad_long, variavel %in% c('desemprego', 'participacao')) %>% 
  ggplot(aes(x=date, y=Valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')

# Plotando gráfico das variáveis em formato longo
agregado_pnad_long %>% 
  ggplot(aes(x=date, y=Valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')

# Dados da PNAD: categoria do emprego

# Obtendo dados sobre categoria do emprego
ocupacao_categoria = 
  get_sidra(api='/t/6320/n1/all/v/4090/p/all/c11913/all') %>% 
  mutate(date = parse_date(`Trimestre Móvel (Código)`, format='%Y%m')) %>% 
  select(date, "Posição na ocupação e categoria do emprego no trabalho principal", Valor) %>% 
  spread("Posição na ocupação e categoria do emprego no trabalho principal", Valor) %>% 
  rename('Empregado sem Carteira' = 'Empregado no setor privado, exclusive trabalhador doméstico - com carteira de trabalho assinada', 
         'Empregado com Carteira' = 'Empregado no setor privado, exclusive trabalhador doméstico - sem carteira de trabalho assinada') %>% 
  as_tibble()

# Transformando os dados em formato longo para plotagem
ocupacao_categoria_long = 
  ocupacao_categoria %>% 
  gather(variavel, Valor, -date)

# Plotando gráfico da categoria do emprego
filter(ocupacao_categoria_long, variavel %in% c('Conta própria', 'Empregado sem Carteira', 'Empregado com Carteira', 'Empregador')) %>% 
  ggplot(aes(x=date, y=Valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none', strip.text = element_text(size=7,face='bold'))

# Dados da PNAD: grupamento de atividades

# Obtendo dados sobre grupamento de atividades
ocupacao_atividades = 
  get_sidra(api='/t/6323/n1/all/v/4090/p/all/c693/all') %>% 
  mutate(date = parse_date(`Trimestre Móvel (Código)`, format='%Y%m')) %>% 
  select(date, "Grupamento de atividades no trabalho principal - PNADC", Valor) %>% 
  spread("Grupamento de atividades no trabalho principal - PNADC", Valor) %>% 
  as_tibble()

# Transformando os dados em formato longo para plotagem
ocupacao_atividades_long =
  ocupacao_atividades %>% 
  gather(variavel, Valor, -date)

# Plotando gráfico de grupamento de atividades
filter(ocupacao_atividades_long, variavel %in% c('Agricultura, pecuária, produção florestal, pesca e aquicultura', 'Comércio, reparação de veículos automotores e motocicletas', 'Construção', 'Indústria geral')) %>% 
  ggplot(aes(x=date, y=Valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none', strip.text = element_text(size=7,face='bold'))

# Dados da PNAD: rendimento

# Obtendo dados sobre rendimento
pnad_rendimento =
  get_sidra(api = '/t/6390/n1/all/v/5929,5933/p/all') %>% 
  mutate(date = parse_date(`Trimestre Móvel (Código)`, format='%Y%m')) %>%
  select(date, "Variável", Valor) %>% 
  spread("Variável", Valor) %>% 
  rename('Rendimento nominal' = "Rendimento médio mensal nominal das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente recebido em todos os trabalhos",
         'Rendimento real' = 'Rendimento médio mensal real das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente recebido em todos os trabalhos') %>% 
  as_tibble()

# Transformando os dados em formato longo para plotagem
pnad_rendimento_long =
  pnad_rendimento %>% 
  gather(variavel, valor, -date)

# Plotando gráfico de rendimento
ggplot(pnad_rendimento_long, aes(x=date, y=valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')

# Dados da PNAD: massa de rendimentos

# Obtendo dados sobre massa de rendimentos
pnad_massa =
  get_sidra(api = '/t/6392/n1/all/v/6288,6293/p/all') %>% 
  mutate(date = parse_date(`Trimestre Móvel (Código)`, format = '%Y%m')) %>% 
  select(date, "Variável", Valor) %>% 
  spread("Variável", Valor) %>% 
  rename('Massa de rendimento nominal' = 'Massa de rendimento mensal nominal das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente recebido em todos os trabalhos',
         'Massa de rendimento real' = 'Massa de rendimento mensal real das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente recebido em todos os trabalhos')

# Transformando os dados em formato longo para plotagem
pnad_massa_long =
  pnad_massa %>% 
  gather(variavel, valor, -date)

# Plotando gráfico de massa de rendimentos
ggplot(pnad_massa_long, aes(x=date, y=valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')