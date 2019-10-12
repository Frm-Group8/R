library(tidyverse) #处理与可视化软件包的集合，包含ggplot2(数据可视化)/dplyr（数据处理）/tidyr（数据清理）/readr（数据提取）/purrr（函数化编程）/tibble（函数类型定义）
library(lubridate) #处理日期时间数据
library(readxl) #读入Excel数据
library(highcharter) #绘制图表
library(tidyquant) #获取数据并在后台加载tidyverse
library(timetk) #处理时间序列
library(tibbletime) #将日价格转换成月收益率
library(quantmod) #金融数据分析
library(PerformanceAnalytics) #量化绩效指标计算和可视化
library(scales) #转换ggplot2绘图所需要的数据
library(magrittr) #管道操作
library(broom) #将统计模型结果整理成数据框形式

#Chapter8
#资本资产定价模型(Capital Asset Pricing Model, CAPM)是由威廉·夏普(William Sharpe)创立的一个模型
#它根据市场的收益和资产与市场的线性关系来估计资产的收益率的回归，这个线性关系就是股票的贝塔系数。
#贝塔可以被认为是股票对市场的敏感性，或相对于市场的风险。   
#CAPM在1964年被引入，并因其创造者而获得诺贝尔奖。
#法玛和弗伦奇曾写道，“CAPM是MBA投资课程的核心”，CAPM一般是这些课程中教授的唯一资产定价模型。
#在实际应用中，该模型的实证记录很差。
#但是计算CAPM betas可以作为更复杂模型的一个很好的模板。
#我们将关注CAPM的一个特殊方面:beta。
#如前所述，贝塔系数是一种资产的贝塔系数，它是将该资产的收益回归到市场收益的结果，它抓住了资产和市场之间的线性关系。
#它是一个很好的工具，，用于建模或根据市场回报回归我们的投资组合回报。


#将投资组合设置为5项资产的组合
#SPY（标准普尔500指数ETF）权重25%
#EFA（非美国股票ETF）加权25%
#IJS（一种小型市值etf）加权20%
#EEM（新兴市场ETF）加权20%
#AGG（债券ETF）加权10%

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
w <- c(0.25, #5项资产的比例，顺序与symbols中的相同
       0.25,
       0.20,
       0.20,
       0.10)

###8.1 CAPM和市场回报率
#输入SPY的价格，计算月回报率，保存为market_returns_xts
#getSymboles函数是取数据，来自quantmod包，数据是在符号“SPY”里面的，提取股票的开盘价、收盘价、调整价、最高价、最低价、成交量
market_returns_xts <-
  getSymbols("SPY",
             src = 'yahoo', 
             from = "2012-12-31", 
             to = "2017-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%      #map函数处理调整价，来自purrr包，Ad(get(.))提取调整价并导入数据
  reduce(merge) %>%      #匹配并拼接多个数据框
  `colnames<-`("SPY") %>%     #命名为SPY
  to.monthly(indexAt = "lastof",    #告诉函数是要索引到每月的第一天还是最后一天，如果想使用第一天，改为indexAt = "firstof"
             OHLC = FALSE) %>%
  Return.calculate(.,
                   method = "log") %>%  #将价格转换成月对数收益率,可以用method="log"得到简单收益率
  na.omit()   #将缺失数据省略

#使用tidyverse时，需要市场回报的tibble变量
#tibble：用来替换data.frame类型的扩展的数据框，与data.frame有相同的语法，使用起来更方便
market_returns_tidy <-
  market_returns_xts %>%
  #将时间序列数据从xts类型转换成tbl类型，命名为"date"
  tk_tbl(preserve_index = TRUE,      
         rename_index = "date") %>%
  na.omit() %>%
  select(date, returns = SPY)  #删去原始数据，仅保留所提到的变量



#从指定的数据库中导入五个ETFs的价格数据，储存在prices的xts对象中
prices <-
  read_excel("/Users/Lixiang/Desktop/Reproducible Finance.xlsx",
             col_types = c("text", "numeric",
                           "numeric", "numeric",
                           "numeric", "numeric")) %>%
  #将时间变量列加入xts中
  mutate(date = ymd(date)) %>%  
  tk_xts(date_var = date)

#使用tidyverse将数据转换为月度回报
asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  #将索引转换为日期
  data.frame(date = index(.)) %>%
  #删除索引，因为它已转换为行名称
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)

#省略NA项
asset_returns_dplyr_byhand <-
  asset_returns_dplyr_byhand %>%
  na.omit()

#tidyverse要求使用long格式或整洁格式的数据，其中每个变量都有自己的列，而不是wide格式
#为了使资产回报整洁，我们需要一个名为“date”的列、一个名为“asset”的列和一个名为“returns”的列
#asset_returns_long有3列，每个列对应一个变量:日期、资产、回报
asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)

#使用tq_portfolio()将asset_returns_long转换为投资组合回报
portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")
    
########}

#因为我们将根据市场回报对投资组合回报进行回归
#要确保投资组合回报的观察值与市场回报的观察值相等
portfolio_returns_tq_rebalanced_monthly %>%
  #在原来的基础上添加一列
  mutate(market_returns = market_returns_tidy$returns) %>%
  head()  #查看数据


########设定变量{

#将日价格转换为月对数收益率
prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
asset_returns_xts <-
  Return.calculate(prices_monthly,  #调用Return.calculate()将回报数据转为log形式
                   method = "log") %>%
  na.omit()
portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

########}


###8.2 计算CAPM的β
#计算资产组合的β
#方法一：β_potpfolio=cov(R_p,R_m)/σ_m
cov(portfolio_returns_xts_rebalanced_monthly,
    market_returns_tidy$returns)/
  var(market_returns_tidy$returns)

#方法二：计算组合中每个资产的β，加权平均得到组合的β
#想对单个资产的回报率和市场回报率进行回归
#希望使用map()通过一次性回归资产回报率和市场回报率
beta_assets <-
  asset_returns_long %>%
  #nest(-asset)改变数据框架，一列为资产名称，一列为每个资产收益率的列表
  nest(-asset)
beta_assets

#使用map()将lm()函数应用于每个列表，并将结果存储在一个名为model的新列中
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .)))  #map:将后面的值返回
beta_assets
beta_assets$model #展开结果，有三列：资产名称、收益率、回归结果（β和截距）

#回归结果储存为S3对象，不是理想的展示对象
#使用来自broom包的tidy()函数清理结果，转换成数据框(model列)
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>%
  mutate(model = map(model, tidy))  #mutate:在后面加一列
beta_assets

#为了可读性，展开model列
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>%
  mutate(model = map(model, tidy)) %>%
  unnest(model) %>%
  mutate_if(is.numeric, funs(round(., 4)))
beta_assets

#使用filter()将β筛选出来，删除截距项
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>%
  mutate(model = map(model, tidy)) %>%
  unnest(model) %>%
  filter(term != "(Intercept)") %>%
  select(-term)
beta_assets

#检验β结果的正确性：SPY与自身的β为1
beta_assets %>%
  select(asset, estimate) %>%
  filter(asset == "SPY")


#现在使用这些资产的加权组合来计算总投资组合贝塔值
beta_byhand <-
  w[1] * beta_assets$estimate[1] +
  w[2] * beta_assets$estimate[2] +
  w[3] * beta_assets$estimate[3] +
  w[4] * beta_assets$estimate[4] +
  w[5] * beta_assets$estimate[5]
beta_byhand  #结果与方法一计算的结果相同


###8.3 在xts中计算CAPM的β
#使用内建函数来确认结果的一致性：PerformanceAnalytics内置的CAPM.beta()函数
#这个函数有两个参数:一个是希望计算β的投资组合的回报率，另一个是市场回报率
beta_builtin_xts <-
  CAPM.beta(portfolio_returns_xts_rebalanced_monthly,
            market_returns_xts)

###8.4 计算tidyverse里CAPM的β
#在tidyverse中查找和显示投资组合beta需要在市场上回归我们的投资组合回报对象，然后用broom清理结果
beta_dplyr_byhand <-
  portfolio_returns_tq_rebalanced_monthly %>%
  do(model =
       lm(returns ~ market_returns_tidy$returns,
          data = .)) %>%
  tidy(model) %>%
  mutate(term = c("alpha", "beta")) %>%
  select(estimate)
beta_dplyr_byhand$estimate[2]  #最后得出的结果与前面一致

###8.5 在tidyquant中计算CAPM的β
#最后，使用tq_performance()将CAPM.beta()应用于returns
beta_builtin_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_return =
           market_returns_tidy$returns) %>%
  na.omit() %>%
  #tq_performance()将投资回报转换为绩效指标
  tq_performance(Ra = returns,
                 Rb = market_return,
                 performance_fun = CAPM.beta) %>%
  `colnames<-`("beta_tq")

beta_builtin_tq %>%
  mutate(dplyr_beta = beta_dplyr_byhand$estimate[2],
         byhand_beta = beta_byhand,
         xts_beta = coredata(beta_builtin_xts)) %>%
  round(3)  #结果与前一致，即回归、手动计算和使用内建函数计算得出的β值都是相同的

###8.6 使用ggplot可视化CAPM
#图8.1：设x轴为市场回报，y轴上为投资组合回报，用散点图使CAPM可视化
#图形展示了很强的线性关系
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  ylab("portfolio returns") +
  xlab("market returns")

#图8.2：使用geom_smooth(method = "lm"， se = FALSE，…)添加一条回归线
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "green") +
  ylab("portfolio returns") +
  xlab("market returns")

#图8.3：图8.2中的回归线的斜率应该等于之前计算的CAPM
#为了确认这一点，散点上加一条线，它的斜率等于之前计算的结果
#y轴截距等于beta_dplyr_byhand中标记的I
#使用geom_abline(...)添加这条线
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns = market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns, y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_abline(aes(
    intercept = beta_dplyr_byhand$estimate[1],
    slope = beta_dplyr_byhand$estimate[2]),
    color = "purple") +
  ylab("portfolio returns") +
  xlab("market returns")

#同时绘制两条线来确认它们是相同的，即它们应该重合
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_abline(
    aes(intercept =
          beta_dplyr_byhand$estimate[1],
        slope = beta_dplyr_byhand$estimate[2]),
    color = "purple") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "green") +
  ylab("portfolio returns") +
  xlab("market returns")


###8.7 扩充我们的数据
#首先计算lm(returns ~ market_returns_tidy$returns…)模型的结果
#augment()带有预测值的原始数据集，存储在一个名为.fit的新列中
portfolio_model_augmented <-
  portfolio_returns_tq_rebalanced_monthly %>%
  do(model =
       lm(returns ~
            market_returns_tidy$returns, data = .)) %>%
  augment(model) %>%
  rename(mkt_rtns = market_returns_tidy.returns) %>%
  select(returns, mkt_rtns, .fitted) %>%
  mutate(date = portfolio_returns_tq_rebalanced_monthly$date)
head(portfolio_model_augmented, 3)

#图8.5：查看拟合的返回值与实际返回值的匹配程度
portfolio_model_augmented %>%
  select(date, returns, .fitted) %>%
  gather(type, data, -date) %>%
  ggplot(aes(x = date, y = data, color = type)) +
  geom_line() +
  xlab("date")


###8.8 使用highcharter包可视化CAPM
#install.packages("highcharter")

#augment()的一个好处是它使我们可以创建一个有趣的高图可视化，该可视化使用图8.2中的回归ggplot复制散点图。
#图8.6CAPM散点图:建立投资组合收益的基本散点图
#该投资组合收益被包含在Portfolio_model_augmented $ returns中，而市场收益则被存储在Portfolio_model_augmented $ mkt_rtns中。
library(highcharter)
highchart() %>% 
  hc_title(text = "Portfolio v. Market Returns Scatter") %>% #表的标题设置为Portfolio v. Market Returns Scatter
  #增加一组数
  hc_add_series(portfolio_model_augmented, #数据框
                type = "scatter", #图表类型设置为散点图
                color = "cornflowerblue", #点的颜色设置
                hcaes(x = round(mkt_rtns, 4), #x轴为投资组合收益，保留四位小数
                      y = round(returns, 4)), #y轴为市场收益，保留四位小数
                name = "Returns") %>% #名称
  hc_xAxis(title = list(text = "Market Returns")) %>%  #将横坐标标题设置为Market Returns
  hc_yAxis(title = list(text = "Portfolio Returns")) %>%   #将横坐标标题设置为Portfolio Returns
  hc_add_theme(hc_theme_flat())  %>% #主题设置为FiveThirtyEight
  hc_exporting(enabled = TRUE) #设置为可输出，可以保存



#图8.7：带日期的CAPM散点图
#给散点图增加一项功能，当鼠标停留在某个点上时显示日期。
#添加日期变量：hc_add_series_scatter（...，date = Portfolio_returns_tq_rebalanced_monthly $ date）
#创建自定义工具提示功能以获取日期：hc_tooltip（formatter = JS（“ function（）{return（'port return：'+ this.y +'<br> mkt return：'+ this.x +'<br> date：'+ this.point.date）}“）。）
highchart() %>% 
  hc_title(text = "Scatter Plot with Date") %>% 
  hc_add_series(portfolio_model_augmented, 
                type = "scatter", 
                color = "cornflowerblue", 
                hcaes(x = round(mkt_rtns, 4), 
                      y = round(returns, 4), 
                      date = date), #添加日期值
                name = "Returns") %>% 
  hc_xAxis(title = list(text = "Market Returns")) %>% 
  hc_yAxis(title = list(text = "Portfolio Returns")) %>% 
  hc_tooltip(formatter = JS("function(){ 
                            return ('port return: ' + this.y + 
                            ' <br> mkt return: ' + this.x +  
                            ' <br> date: ' + this.point.date)}")) %>%  #设置提示框的格式
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)



#图8.8带回归线的CAPM散点图
#添加回归线，添加到expand()函数中为拟合的回归线设置x坐标（市场收益）和y坐标（拟合值)
#添加hc_add_series（portfolio_model_augmented，type =“ line”，hcaes（x = mkt_rtns，y = .fitted））
highchart() %>% 
  hc_title(text = "Scatter with Regression Line") %>% 
  hc_add_series(portfolio_model_augmented, 
                type = "scatter", 
                color = "cornflowerblue",
                hcaes(x = round(mkt_rtns, 4), 
                      y = round(returns, 4), 
                      date = date), 
                name = "Returns") %>% 
  hc_add_series(portfolio_model_augmented, #添加另一组数
                type = "line", #图表类型设置为线
                hcaes(x = mkt_rtns, #x值设置为市场收益
                      y = .fitted), #y值设置为拟合值
                name = "CAPM Beta = Regression Slope") %>% #名称为CAPM Beta = Regression Slope
  hc_xAxis(title = list(text = "Market Returns")) %>% 
  hc_yAxis(title = list(text = "Portfolio Returns")) %>% 
  hc_tooltip(formatter = JS("function(){ 
                            return ('port return: ' + this.y + 
                            ' <br> mkt return: ' + this.x + 
                            ' <br> date: ' + this.point.date)}")) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)



####8.9 使用Shiny包制作CAPM App 
#CAPM shiny程序允许用户构建投资组合并计算市场Beta，同时还可以将投资组合收益与市场收益进行比较。
#我们将显示分散在市场上的投资组合收益，并画出回归线。 我们还将显示alpha和beta项的估计值以及p值。 我们的输入侧边栏和投资组合收益计算是我们的标准。 与我们的SharpeRatioapp类似，我们计算市场收益，但将其转换为小标题以用作调用lm（）的自变量。
---
title: "Capm Beta"
runtime: shiny
output:
  flexdashboard::flex_dashboard:
    orientation: rows
    source_code: embed
---


```{r setup, message = FALSE}
library(tidyverse)
library(highcharter)
library(tidyquant)
library(timetk)
library(scales)
library(broom)
library(highcharter)
library(DBI)
library(RMySQL)
#加载需要的包
```

Sidebar {.sidebar}
=====================================
  
```{r}
#建立侧栏并允许用户输入五个股票名称和权重
fluidRow(
  column(6,
  textInput("stock1", "Stock 1","600000")),#创建长度为6的列，输入数据定义变量名为stock1，文本框名为Stock1，文本输入，默认值为SPY
  column(5,
  numericInput("w1", "Portf. %", 25, min = 1, max = 100))#创建长度为5的列，输入数据定义变量名为w1，文本框名为Stock1,数值输入，最小为1最大为100.默认值为25
)  
# 重复五次
fluidRow(
  column(6,
  textInput("stock2", "Stock 2", "600004")),
  column(5,
  numericInput("w2", "Portf. %", 25, min = 1, max = 100))
)

fluidRow(
  column(6,
  textInput("stock3", "Stock 3", "600006")),
  column(5,
  numericInput("w3", "Portf. %", 20, min = 1, max = 100))
)

fluidRow(
  column(6,
  textInput("stock4", "Stock 4", "600007")),
  column(5,
  numericInput("w4", "Portf. %", 20, min = 1, max = 100))
)

fluidRow(
  column(6,
  textInput("stock5", "Stock 5", "600015")),
  column(5,
  numericInput("w5", "Portf. %", 10, min = 1, max = 100))
)
#创建开始日期行
fluidRow( #创建行
  column(7,
  dateInput("date", "Starting Date", "2019-01-01", format = "yyyy-mm-dd"))
)
#创建长度为7的列，输入数据定义变量名为data，文本框名为Starting Date,日期输入，默认值为2013-01-01，形式为yyyy-mm-dd
actionButton("go", "Submit") #提交按钮
```


```{r}
prices <- eventReactive(input$go, {
    #等待按钮被点击，得到价格数据
  symbols <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
  data<-list()
for (i in 1:5){ 
  mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') #和本地数据库建立连接
  SQL_statement<-paste("SELECT  `day`,`close` 
FROM `cn_stock_quote`.`daily_adjusted_quote`
WHERE code=",symbols[i],"and day >='",input$date,"'
ORDER BY 'day' DESC ") #从中国股票的每日调整报价表中以股票代码和开始日期选择获取日期、收盘价收盘价
  aa <- dbGetQuery(mydb,SQL_statement)
  colnames(aa)[2]<-paste("x",symbols[i],sep="",collapse="")
  data[[i]]=aa
}#用循环构建list，每个参数对应一张数据表
  stockdata<-data %>%
  reduce(merge)#合并表格
  prices<-xts(stockdata[,-1],order.by = as.Date(stockdata[,1]))  #转换为xts数据类型
})


market_return <- eventReactive(input$go, { #等待按钮被点击，得到市场收益率数据
  
     mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
  SQL_statement<-paste("SELECT  `trade_date`,`last` 
FROM `cn_stock_index`.`daily_quote`
WHERE index_code='000001' and trade_date >='",input$date,"'
ORDER BY 'trade_date' DESC ")
  bb <- dbGetQuery(mydb,SQL_statement)
market_prices<-xts(bb[,-1],order.by = as.Date(bb[,1]))
colnames(market_prices)<- colnames(bb)[2]<-paste("x","000001",sep="",collapse="")#获取上证综合指数计算市场收益率
  market_return<-market_prices%>%
    to.monthly(indexAt = "lastof",    #告诉函数是要索引到每月的第一天还是最后一天，如果想使用第一天，改为indexAt = "firstof"
             OHLC = FALSE) %>%
    Return.calculate(.,
                     method = "log") %>%  #将价格转换成月对数收益率,可以用method="log"得到简单收益率
    na.omit()  %>%
    tk_tbl(preserve_index = TRUE,      
                      rename_index = "date") %>%
    select(date, returns = x000001) 

})

portfolio_returns_tq_rebalanced_monthly <- eventReactive(input$go, {
    #返回资产组合调整后的月度收益率
  prices <- prices()
  #需要权重之和等于100，不然就报错，提示The portfolio weights must sum to 100%!
  validate(need(input$w1 + input$w2 +
                input$w3 + input$w4 +
                input$w5 == 100,
                "The portfolio weights must sum to 100%!"))
   #调用价格函数
  w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100, input$w5/100)
  #令w定义为输入的权重向量
  portfolio_returns_tq_rebalanced_monthly <- 
      prices %>% #输入之前获得的各股票价格
      to.monthly(indexAt = "lastof", OHLC = FALSE) %>% #转换为月度数据
      tk_tbl(preserve_index = TRUE, rename_index = "date") %>% #将价格从xts转换为tibble
      gather(asset, prices, -date) %>% #将宽格式转换为长格式
      group_by(asset) %>%   #按资产列分组
      mutate(returns = (log(prices) - log(lag(prices)))) %>%  #返回对数收益率的结果
      na.omit() %>% #删除空值
      tq_portfolio(assets_col  = asset,  #资产列
               returns_col = returns,#收益列
               weights     = w,#权重
               col_rename  = "returns",#重命名
               rebalance_on = "months")#按月重新平衡
}) #获得投资组合的收益率


beta_dplyr_byhand <- eventReactive(input$go, {
  #计算投资组合的beta值
  #portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
  #调用获取投资组合的调整后的月度收益率数据函数
  market_return <- market_return()
   #调用获取市场收益率的函数
  #定义beta值的计算方式
beta_dplyr_byhand <- 
  portfolio_returns_tq_rebalanced_monthly() %>% 
  do(model = lm(returns ~ market_return$returns, data = .))#模型选取一元线性回归方程，，自变量市场收益率，因变量投资组合的收益率，计算beta值

})
  
  
portfolio_model_augmented <- eventReactive(input$go, {
  #生成含预测数据的新数据框
  portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
  #调用获取投资组合的调整后的月度收益率数据函数
  beta_dplyr_byhand() %>% #调用计算beta的函数
  augment(model) %>% #将一元线性回归模型预测的数据增加到原数据中，并存储在名为.fitted的新列
  rename(mkt_rtns = market_return.returns) %>%  #重命名mkt_rtns
  select(returns, mkt_rtns, .fitted) %>% #保存收益率，市场收益率，预测收益率列
  mutate(date = portfolio_returns_tq_rebalanced_monthly$date)#返回索引为投资组合的调整后的月度收益率
  
})


```

Click submit to calculate the CAPM Beta of your portfolio
=====================================  

Row {data-height=650}
-----------------------------------------------------------------------

### CAPM Highcharter

```{r}
renderHighchart({
#创建Highchart图
portfolio_model_augmented <- portfolio_model_augmented()
#赋值
highchart() %>% 
  hc_title(text = "Scatter with Regression Line") %>% 
  hc_add_series(portfolio_model_augmented,  #增加一组数
                type = "scatter",#图表类型设置为散点图
                color = "cornflowerblue",#点的颜色设置
                hcaes(x = round(mkt_rtns, 4), #x轴为投资组合收益，保留四位小数
                      y = round(returns, 4),#y轴为市场收益，保留四位小数
                      date = date), #添加日期值
                name = "Returns") %>% #名称
  hc_add_series(portfolio_model_augmented, #添加回归线,添加另一组数
                type = "line", #图表类型设置为线
                enableMouseTracking = FALSE,
                hcaes(x = mkt_rtns, y = .fitted), #x值设置为市场收益,y值设置为拟合值
                name = "CAPM Beta = Slope of Line") %>% 
  hc_xAxis(title = list(text = "Market Returns")) %>% #将横坐标标题设置为Market Returns
  hc_yAxis(title = list(text = "Portfolio Returns")) %>% #将横坐标标题设置为Portfolio Returns
  hc_tooltip(formatter = JS("function(){
     return ('port return: ' + this.y + ' <br> mkt return: ' + this.x +  
     ' <br> date: ' + this.point.date)}"))%>% #创建自定义工具提示功能以获取日期
  hc_add_theme(hc_theme_flat()) %>% #主题设置为FiveThirtyEight
  hc_exporting(enabled = TRUE) #设置为可输出，可以保存


})
```


Row 2 {data-height=350}
----------------------------------

### Model Results

```{r}
renderTable({ #创建表格
  beta_dplyr_byhand() %>% #返回计算的beta值
  tidy(model) %>% #清理结果，来自broom包
  mutate(term = c("alpha", "beta")) #返回alpha和beta两行值
}, digits = 4)#显示4位小数
```
