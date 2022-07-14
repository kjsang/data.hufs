
# 1. packages load --------------------------------------------------------

pacman::p_load(
  tidyverse, # 타이디
  rstatix, # 통계툴
  palmerpenguins, # 데이터셋
  RColorBrewer, 
  naniar, # useful for visualizing missing values
  PerformanceAnalytics, # 왜도 및 첨도
  psych # 기하평균, 조화평균 구하기
  )


# 2. data load ------------------------------------------------------------

data <- palmerpenguins::penguins
data %>% head()
data %>% tail()
data %>% summary() # 각 변수별로 NA 값이 얼마나 포함되어 있는지 알 수 있다. NA값을 삭제하고 진행할 것인지, 아니면 대체하고 진행할 것인지는 분석가의 판단이다. 나는? 일단 살펴보고...


# 2.1. data preprocessing -------------------------------------------------
data %>% vis_miss() # NA 시각화해보니 별로 없다. 그냥 지우자.
data %>% na.omit() -> data_prep
data_prep %>% vis_miss() # 깔끔하다.


# 2.2. 데이터 특성 파악: 왜도 및 첨도 -------------------------------------------------
data_prep %>% 
  filter(species == "Adelie") %>% 
  summarise(
    왜도 = PerformanceAnalytics::skewness(bill_length_mm),
    첨도 = PerformanceAnalytics::kurtosis(bill_length_mm)
  )
# 살짝 평평하고 살짝 왼쪽으로 치우친 분포

data_prep %>% 
  filter(species == "Adelie") %>% 
  get_summary_stats(bill_length_mm)

# n: the number of individuals
# min: minimum
# max: maximum
# median: median
# mean: mean
# q1, q3: the first and the third quartile, respectively.
# iqr: interquartile range
# mad: median absolute deviation (see ?MAD)
# sd: standard deviation of the mean
# se: standard error of the mean
# ci: 95 percent confidence interval of the mean

# CV 구하는 방법: sd / mean * 100
data_prep %>% 
  filter(species == "Adelie") %>% 
  summarise(
    CV = sd(bill_length_mm)/mean(bill_length_mm)*100
  )



data_prep %>% 
  filter(species == "Adelie") %>% 
  ggplot(aes(bill_length_mm)) +
  geom_histogram(alpha = 1, colour = "cornflowerblue",
                 bins = 20) +
  annotate("segment", x = 38.8, xend = 38.8, y = 0, yend = 18,
           colour = "red", size = 2) + # 평균
  annotate("segment", x = 38.8, xend = 38.8, y = 0, yend = 18,
           colour = "blue", size = 1) # 중앙값
#평균과 중앙값이 같으나, 분포는 다를 수 있고, 이에 따라 왜도가 차이가 날 수 있다.

data_prep %>% 
  filter(species == "Adelie") %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(color = species),
               width = 0.3) + 
  coord_flip() # 왼쪽에 데이터가 더 많다.

# 3. Visualization --------------------------------------------------------


# 3.1. 변수별 분포(점도표) -------------------------------------------------------------

# 시각화를 해보자. 하지만 그 전에 g를 kg로 바꿔주자
data_prep %>% 
  mutate(body_mass_kg = body_mass_g / 1000) -> data_prep2

data_prep2 %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_kg)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 1) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  xlab("날개길이") +
  ylab("몸무게(kg)")
# 시각화를 해보니 딱히 아델리랑 친스트랩이 구분이 안 된다. 다른 변수로도 살펴보자.

data_prep2 %>% 
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 1) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  xlab("날개길이") +
  ylab("부리길이")
# 부리길이와 날개길이를 보니 어느정도 구분이 된다.


# 3.2. 히스토그램 --------------------------------------------------------------
data_prep2 %>% 
  ggplot(aes(flipper_length_mm)) +
  geom_histogram(aes(fill = species),
                     alpha = 0.5,
                     position = "identity") + # 스택으로 하면 쌓이는데, 개별값으로 하면 안 쌓인다.
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  xlab("날개길이") +
  ylab("빈도")
theme(base_family = "Nanumgothic")

# 3.3. 박스플롯 ---------------------------------------------------------------
data_prep2 %>% 
  ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species),
               width = 0.3) +
  geom_jitter(aes(color = species),
              alpha = 0.5,
              position = position_jitter(width = 0.2, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  xlab("종") +
  ylab("날개길이")


# 4. 상관관계분석 ---------------------------------------------------------------

data_cor <- mtcars
data_cor %>% glimpse()

# [, 1]	mpg	Miles/(US) gallon -> Interval
# [, 2]	cyl	Number of cylinders 
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight) 
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears

theme_set(theme_grey(base_family='AppleGothic'))
# 4.1. 피어슨 상관분석 -----------------------------------------------------------
data_cor %>% 
  rstatix::cor_test(vars = mpg, vars2 = disp,
                    alternative = "two.sided",
                    method = "pearson",
                    conf.level = 0.95)

# 4.2. 스피어만 상관분석: 순서 대 순서 ----------------------------------------------------------
data_cor %>% 
  rstatix::cor_test(vars = cyl, vars2 = gear,
                    alternative = "two.sided",
                    method = "spearman",
                    conf.level = 0.95)

# 4.3. 켄달타우: 순서 대 순서 ---------------------------------------------------------------
data_cor %>% 
  rstatix::cor_test(vars = cyl, vars2 = gear,
                    alternative = "two.sided",
                    method = "kendall",
                    conf.level = 0.95)


# 4.4. 점이연 상관계수: 연속 대 명목 --------------------------------------------------
data_cor %>% 
  rstatix::cor_test(vars = mpg, vars2 = am,
                    alternative = "two.sided",
                    method = "pearson", # 점이연은 피어슨과 계산이 같다
                    conf.level = 0.95)

# 4.5. 이연 상관계수: 연속 대 명목(원래는 연속이었던) --------------------------------------------------
data_cor %>% 
  get_summary_stats(mpg) # 평균이 20이니, 20이상은 1, 20이하는 0으로 코딩

data_cor %>% 
  mutate(mpg_prep = ifelse(mpg >= 20, 1, 0)) %>% # 20 이상은 1, 20이하는 0으로 코딩
  rstatix::cor_test(vars = mpg_prep, vars2 = disp,
                    alternative = "two.sided",
                    method = "pearson", # 이연은 피어슨과 계산이 같다
                    conf.level = 0.95)

# 4.6. 파이 상관계수: 명목 대 명목 --------------------------------------------------
data_cor %>% 
  rstatix::cor_test(vars = vs, vars2 = am,
                    alternative = "two.sided",
                    method = "pearson", # 파이는 피어슨과 계산이 같다
                    conf.level = 0.95)

#### 기타 ####
# Tetrachoric 상관계수– 범주들간의 상관계수이나, 범주들이 인위적으로 이분화된 경우에 사용하는 이다.– 이분화되기 전 원래 변수는 정규분포를 띠고 있다고 가정한다.


# 5. 평균 -------------------------------------------------------------------
read_csv("0.data//1.Korea_GDP.csv") -> GDP
GDP %>% 
  pivot_longer(cols = `1970`:`2021`,
               names_to = "Year",
               values_to = "GDP") -> GDP_prep # 데이터 적재

# 5.2. 기하평균 & 조화평균 ---------------------------------------------------------------
GDP_prep %>% 
  summarise(
    H = harmonic.mean(GDP), # 31372
    M = mean(GDP), # 665664.
    G = geometric.mean(GDP) # 230684.
  ) # 순서는? M > G > H

GDP_prep %>% 
  arrange(Year) %>% 
  mutate(Year = as.integer(Year),
         Diff_year = Year - lag(Year),
         Diff_growth = GDP - lag(GDP),
         Rate_percent = (Diff_growth / Diff_year)/GDP*100) -> GDP_growth
GDP_growth %>% 
  filter(!Year == 1970) %>% 
  summarise(
    G = geometric.mean(Rate_percent) # 평균 GDP 증가율
  )

# 내가 산에 올라갈 때에는 시속 6킬로, 내려올 때에는 시속 12킬로로 왔다면 나의 평균 속력은?
mountain <- c(6, 12)
harmonic.mean(mountain) # 평균시속 8 (평균 9가 아니다)

# 점점 속력이 불규칙적으로 증가하는 자동차 (각 데이터는 키로당 평균 속력)
car <- c(10,20,30,34,38,
         40,50,70,90,100,
         100,105)
harmonic.mean(car) # 이 자동차는 12킬로를 평균 35.4km/h로 간 것
