#繪圖套件
library(ggplot2)
library(plyr)
library(lattice)
library(Rmisc)

#1.估計2330台積電的股價變動標準差
data=read.csv("C:\\Users\\ryanl\\Desktop\\2330.csv",header = TRUE) #2000/1/4至2019/11/22之價量日資料
n_d_est=500 #以2017/11/08至2019/11/22共500個交易日計算
x=matrix(rep(0,n_d_est*2),2,n_d_est)
end_date=4942 #end_date=2019/11/22
PV_sigma_est=x
for (j in 1:n_d_est){
  PV_sigma_est[1,j]=abs(logb((data$close[end_date-n_d_est+j]/data$close[end_date-n_d_est+j-1])))
} #計算|ln(p(t+1)/p(t))|之標準差

#2.設定模擬次數與空白矩陣
sim=1000 #模擬價量1000次
costdiff=matrix(rep(0,n_d*7),7,sim) #成本差
startbuyday=matrix(rep(0,n_d*7),7,sim) #開始買進日
endbuyday=matrix(rep(0,n_d*7),7,sim) #結束買進日
totalbuyday=matrix(rep(0,n_d*7),7,sim) #總買進天數
lastdayprice=matrix(rep(0,n_d*7),7,sim) #最後模擬日之模擬價格

#3.設定市場走勢(牛熊/市)與價量關係(ex.價漲時量同向增加)
bullbear=1 # 1牛市/-1熊市
updown=1 # 1價漲/-1價跌
direction=1 # 1價量同向/-1反向

#4.模型設定
n_d=200 #模擬200日價量
P0=data$close[end_date] #第1日價格:309
V0=data$volume[end_date] #第1日交易量:26492
sigma=sd(PV_sigma_est[1,]) #sigma0:ln(價格變動)的起始標準差
sigma.v=sigma #假設交易量標準差=價格標準差
prob_rise=0.5 #價格上漲機率=0.5(漲跌隨機) 
prob_rise.v=0.5 #交易量上漲機率=0.5(漲跌隨機)
threshold=0.05 #若 (p(t)/p(t-1))<(1-thrd) 或 (p(t)/p(t-1))>(1+thrd), 價量同向變動
#iGARCH process參數設定
omega=10^-5 #論文假設omega=0.01過大，本模擬假設較小數值
omega.v=10^-5
beta=0.65 #論文給定
beta.v=0.65
alpha=0.14 #論文給定
alpha.v=0.14
gamma=0.21-omega #本模擬假設扣除之omega值加入gamma值
gamma.v=0.21-omega.v
#價量模擬模型
for (li in 1:7){ #(li,每日買進上限)=(1,4%)；(2,4.5%)；...；(7,7%)
  for (n in (1:sim)){ 
    repeat{ #若模擬結果不符合需求條件則重新模擬至符合條件
      X=matrix(rep(0,n_d*2),2,n_d) #(2*n_d)空白矩陣
      rdm=rnorm(n_d*2) #(2*n_d)隨機ND(0,1)矩陣
      eta=matrix(rdm,2,n_d) 
      epsilon=X 
      epsilon[,1]=c(sigma*eta[1,1],sigma.v*eta[2,1]) #設定 epsilon0
      sigma2=X #sigma^2 matrix
      sigma2[,1]=c(sigma^2,sigma.v^2) #設定 sigma^2,0
      for (j in 2:n_d){ #根據iGARCH計算sigma^2 & epsilon
        sigma2[1,j]=max(omega+alpha*epsilon[1,j-1]^2+beta*sigma2[1,j-1]+gamma.v*(epsilon[2,j-1])^2, 0)
        sigma2[2,j]=max(omega.v+alpha.v*epsilon[2,j-1]^2+beta.v*sigma2[2,j-1]+gamma*(epsilon[1,j-1])^2, 0)
        epsilon[1,j]=sigma2[1,j]^0.5*eta[1,j]
        epsilon[2,j]=sigma2[2,j]^0.5*eta[2,j]
      }
      rdm01=runif(n_d*2, min=0, max=1) 
      Rdm01=matrix(rdm01,2,n_d) #(2*n_d)隨機UNI(0,1)矩陣:用於隨機決定價量漲跌方向
      UPorDOWN=X 
      for (j in 2:n_d){
        if(Rdm01[1,j]<=prob_rise){UPorDOWN[1,j]=1}else{UPorDOWN[1,j]=-1}
        if(Rdm01[2,j]<=prob_rise.v){UPorDOWN[2,j]=1}else{UPorDOWN[2,j]=-1}
      } #(2*n_d)矩陣，決定每日價量漲跌方向，上漲則元素=1/下跌則元素=-1
      
      PVchg=X #(2*n_d)矩陣，紀錄每日|ln(p(t+1)/p(t))|&|ln(v(t+1)/v(t))|
      PVchg[,1]=c(0,0) #假設第一天價量未變動
      for (j in 2:n_d){
        PVchg[1,j]=
          abs(max(min(rnorm(1,mean=PVchg[1,j-1],sd=sigma2[1,j-1]^0.5), 
                      logb(1.1)),logb(0.9)))*UPorDOWN[1,j]
        #ND(mean=|ln(p(t+1)/p(t))|,sd=sigma(t)) *+/-1 = ln(p(t+2)/p(t+1)，變動在10%以內
        if(((PVchg[1,j])>logb(1+threshold)|PVchg[1,j]<logb(1-threshold)) #價量關係條件一:判斷漲跌幅是否超過門檻值(5%)
           & (PVchg[1,j])*updown>0  #價量關條件二:判斷當日價格上漲或下跌
        ){UPorDOWN[2,j]=UPorDOWN[1,j]*direction} #根據價量關條件控制價量同向或反向
        PVchg[2,j]=abs(rnorm(1,mean=PVchg[2,j-1],sd=sigma2[2,j-1]^0.5))*UPorDOWN[2,j]
        #ND隨機產生成交量變化(同價格方法)，控制成交量漲跌方向 
      }
      
      PV=X #(2*n_d)矩陣，紀錄每日價量
      PV[,1]=c(P0,V0) #首日價格為真實數據
      for (j in 2:n_d){
        PV[1,j]=round(PV[1,j-1]*exp(PVchg[1,j]), digits=2) #價格至小數點後2位
        PV[2,j]=round(PV[2,j-1]*exp(PVchg[2,j])) #成交量至個位數
      }
      #簡單MA策略
      fast=10 #短均線10日
      slow=30 #長均線30日
      zeros=rep(0,n_d*3)
      MA=matrix(zeros,3,n_d) #(3*n_d)矩陣，逐列紀錄MA(10)/MA(30)/交易訊號 
      for (j in (1+fast):n_d){ #從第11日開始計算前10日均價
        tempf=0
        for (f in 1:fast){
          tempf=tempf+PV[1,j-f]
        }
        tempf=tempf/fast
        MA[1,j]=tempf
      }
      for (j in (1+slow):n_d){ #從第31日開始計算前30日均價
        temps=0
        for (s in 1:slow){
          temps=temps+PV[1,j-s]
        }
        temps=temps/slow
        MA[2,j]=temps
      }
      for (j in (1+slow):n_d){
        if(MA[1,j]>MA[2,j]){MA[3,j]=1}else{MA[3,j]=-1} #MA(10)>MA(30)則計為1，反之為-1
      }
      #performence
      target_lot=30000 #目標買進張數
      trans_fee=0.001425 #交易費用率
      fee_discount=0.5 #交易費用折扣
      buyupper=(li+7)/200 #設定買進上限:4%/4.5%/.../7%
      buylower=0 #設定買進下陷
      quota=runif(n_d,min=buylower,max=buyupper) #隨機產生買進上下限內的均勻分配
      Trans=matrix(rep(0,n_d*4),4,n_d)
      #(4*n_d)矩陣，紀錄:部位淨值/當天買幾張/當天已累積幾張/目前平均買入成本
      
      for (j in slow:n_d){
        if(MA[3,j-1]==-1&MA[3,j]==1){ #當出現第一次買進訊號(快線向上穿越慢線)
          for (k in j:n_d){
            if((round(PV[2,k]*quota[k])+Trans[3,k-1])<=target_lot){Trans[2,k]=round(PV[2,k]*quota[k])}
            #累積張數小於目標張數則按上限隨機買進
            else{Trans[2,k]=target_lot-Trans[3,k-1]} #累積張數不可超過目標張數
            Trans[3,k]=Trans[3,k-1]+Trans[2,k] #計算累計張數
            Trans[1,k]=Trans[1,k-1]-(Trans[2,k]*PV[1,k]*(1+trans_fee*(1-fee_discount)))+(Trans[3,k]*PV[1,k])
            #買進總支出(含交易費用)
            Trans[4,k]=(Trans[4,k-1]*Trans[3,k-1]+Trans[2,k]*PV[1,k])/(Trans[3,k]+10^-8) 
            #計算平均成本(分母+10^-8為排除分母為0之情況)
          } 
        }
      }
      record=rep(0,3) #3元素向量，紀錄:1st買進訊號出現當天股價/買進開始日/買進結束日
      for (j in 2:n_d){
        if(Trans[4,j-1]==0 & Trans[4,j]!=0){
          record[1]=Trans[4,j]
          record[2]=j-slow-1
        }
        if(Trans[2,j-1]!=0 & Trans[2,j]==0){
          record[3]=j-slow-1
        }
      }
      #以下部分:若符合需求條件，才紀錄該次模擬結果，否則重新模擬
      if(record[2]>0 #有確實買進
         & Trans[3,n_d]==target_lot #有確實在n_d天買完
     
         & PV[1,50]*bullbear<PV[1,1]*bullbear 
         & PV[1,100]*bullbear<PV[1,50]*bullbear
         & PV[1,150]*bullbear<PV[1,100]*bullbear
         & PV[1,200]*bullbear<PV[1,150]*bullbear
         & PV[1,record[3]+1]*bullbear>PV[1,1]*(2^bullbear)*bullbear
         #以上設定牛市中每50日的模擬價格在上升，但買進結束日隔日之價格不超過第1日之2倍(符合現實情況)，或者
         #熊市中每50日的模擬價格在下跌，但買進結束日隔日之價格不低於第1日之0.5倍
        ){
        costdiff[li,n]=Trans[4,n_d]-record[1] #紀錄成本差=平均成本-1st買進訊號出現當天股價
        startbuyday[li,n]=record[2] #買進開始日
        endbuyday[li,n]=record[3] #買進結束日
        totalbuyday[li,n]=record[3]-record[2] #總買進天數
        lastdayprice[li,n]=PV[1,n_d] #第200日價格
        print(li) #模擬時看跑到哪個買進上限
        print(n) #模擬時看跑到第幾次模擬
        print(Trans[4,n_d]) 
        print(record)
        break #模擬成功，紀錄結果並繼續下次模擬
      }
      
    }
  }
}
#成本差盒狀圖
costdiffdata=data.frame("4%"=costdiff[1,],"4.5%"=costdiff[2,],"5%"=costdiff[3,],
                        "5.5%"=costdiff[4,], "6%"=costdiff[5,],"6.5%"=costdiff[6,],"7%"=costdiff[7,])
boxplot(costdiffdata,xlab = "買進上限(%)",ylab = "買進成本差",outline=FALSE)
title("牛市+價漲量增") #標題
grid() #格線
#買進開始日盒狀圖
startbuydaydata=data.frame("4%"=startbuyday[1,],"4.5%"=startbuyday[2,],"5%"=startbuyday[3,],
                           "5.5%"=startbuyday[4,],"6%"=startbuyday[5,],"6.5%"=startbuyday[6,],"7%"=startbuyday[7,])
boxplot(startbuydaydata,xlab = "買進上限(%)",ylab = "開始買進日",outline=FALSE)
title("牛市+價漲量增")
grid()
#買進結束日盒狀圖
endbuydaydata=data.frame("4%"=endbuyday[1,],"4.5%"=endbuyday[2,],"5%"=endbuyday[3,],
                         "5.5%"=endbuyday[4,],"6%"=endbuyday[5,],"6.5%"=endbuyday[6,],"7%"=endbuyday[7,])
boxplot(endbuydaydata,xlab = "買進上限(%)",ylab = "停止買進日",outline=FALSE)
title("牛市+價漲量增")
grid()
#總買進天數盒狀圖
totalbuydaydata=data.frame("4%"=totalbuyday[1,],"4.5%"=totalbuyday[2,],"5%"=totalbuyday[3,],
                           "5.5%"=totalbuyday[4,],"6%"=totalbuyday[5,],"6.5%"=totalbuyday[6,],"7%"=totalbuyday[7,])
boxplot(totalbuydaydata,xlab = "買進上限(%)",ylab = "總買進天數",outline=FALSE)
title("牛市+價漲量增")
grid()
#第200日模擬價格盒狀圖
lastdaypricedata=data.frame("4%"=lastdayprice[1,],"4.5%"=lastdayprice[2,],"5%"=lastdayprice[3,],
                            "5.5%"=lastdayprice[4,],"6%"=lastdayprice[5,],"6.5%"=lastdayprice[6,],"7%"=lastdayprice[7,])
boxplot(lastdaypricedata,xlab = "買進上限(%)",ylab = "200日後價格",outline=FALSE)
grid()

#單次模擬組圖
modeldata=data.frame(date=c(0:(n_d-slow-1)),Price=PV[1,(slow+1):n_d],Volume=PV[2,(slow+1):n_d], 
                     Fast=MA[1,(slow+1):n_d], Slow=MA[2,(slow+1):n_d], Signal=MA[3,(slow+1):n_d], 
                     AvgCost=Trans[4,(slow+1):n_d], lots_t=Trans[2,(slow+1):n_d], cum_lots=Trans[3,(slow+1):n_d])
#價格+MA
p1=ggplot(data=modeldata) +
  geom_line(mapping = aes(y = Price, x = date), colour = "red", size = 1) +
  geom_line(mapping = aes(y = Fast, x = date), colour = "blue", size = 0.5) +
  geom_line(mapping = aes(y = Slow, x = date), colour = "black", size = 0.5)
#交易量
p2=ggplot(data=modeldata) +
  geom_bar(mapping = aes(y = Volume, x = date), stat = "identity", size = 0.01, fill = "skyblue1")
#平均成本
p3=ggplot(data=modeldata) +
  geom_line(mapping = aes(y = AvgCost, x = date), colour = "black", size = 0.5)
#當日買進張數+累積買進張數
p4=ggplot(data=modeldata) +
  geom_line(mapping = aes(y = cum_lots, x = date), colour = "red", size = 0.5) +
  geom_line(mapping = aes(y = lots_t, x = date), colour = "black", size = 0.5)
multiplot(p1,p2,p3,p4, cols = 2) #2*2多圖