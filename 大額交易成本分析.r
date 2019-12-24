#ø�ϮM��
library(ggplot2)
library(plyr)
library(lattice)
library(Rmisc)

#1.���p2330�x�n�q���ѻ��ܰʼзǮt
data=read.csv("C:\\Users\\ryanl\\Desktop\\2330.csv",header = TRUE) #2000/1/4��2019/11/22�����q����
n_d_est=500 #�H2017/11/08��2019/11/22�@500�ӥ����p��
x=matrix(rep(0,n_d_est*2),2,n_d_est)
end_date=4942 #end_date=2019/11/22
PV_sigma_est=x
for (j in 1:n_d_est){
  PV_sigma_est[1,j]=abs(logb((data$close[end_date-n_d_est+j]/data$close[end_date-n_d_est+j-1])))
} #�p��|ln(p(t+1)/p(t))|���зǮt

#2.�]�w�������ƻP�ťկx�}
sim=1000 #�������q1000��
costdiff=matrix(rep(0,n_d*7),7,sim) #�����t
startbuyday=matrix(rep(0,n_d*7),7,sim) #�}�l�R�i��
endbuyday=matrix(rep(0,n_d*7),7,sim) #�����R�i��
totalbuyday=matrix(rep(0,n_d*7),7,sim) #�`�R�i�Ѽ�
lastdayprice=matrix(rep(0,n_d*7),7,sim) #�̫�����餧��������

#3.�]�w��������(����/��)�P���q���Y(ex.�����ɶq�P�V�W�[)
bullbear=1 # 1����/-1����
updown=1 # 1����/-1���^
direction=1 # 1���q�P�V/-1�ϦV

#4.�ҫ��]�w
n_d=200 #����200����q
P0=data$close[end_date] #��1�����:309
V0=data$volume[end_date] #��1�����q:26492
sigma=sd(PV_sigma_est[1,]) #sigma0:ln(�����ܰ�)���_�l�зǮt
sigma.v=sigma #���]����q�зǮt=����зǮt
prob_rise=0.5 #����W�����v=0.5(���^�H��) 
prob_rise.v=0.5 #����q�W�����v=0.5(���^�H��)
threshold=0.05 #�Y (p(t)/p(t-1))<(1-thrd) �� (p(t)/p(t-1))>(1+thrd), ���q�P�V�ܰ�
#iGARCH process�ѼƳ]�w
omega=10^-5 #�פ尲�]omega=0.01�L�j�A���������]���p�ƭ�
omega.v=10^-5
beta=0.65 #�פ嵹�w
beta.v=0.65
alpha=0.14 #�פ嵹�w
alpha.v=0.14
gamma=0.21-omega #���������]������omega�ȥ[�Jgamma��
gamma.v=0.21-omega.v
#���q�����ҫ�
for (li in 1:7){ #(li,�C��R�i�W��)=(1,4%)�F(2,4.5%)�F...�F(7,7%)
  for (n in (1:sim)){ 
    repeat{ #�Y�������G���ŦX�ݨD����h���s�����ܲŦX����
      X=matrix(rep(0,n_d*2),2,n_d) #(2*n_d)�ťկx�}
      rdm=rnorm(n_d*2) #(2*n_d)�H��ND(0,1)�x�}
      eta=matrix(rdm,2,n_d) 
      epsilon=X 
      epsilon[,1]=c(sigma*eta[1,1],sigma.v*eta[2,1]) #�]�w epsilon0
      sigma2=X #sigma^2 matrix
      sigma2[,1]=c(sigma^2,sigma.v^2) #�]�w sigma^2,0
      for (j in 2:n_d){ #�ھ�iGARCH�p��sigma^2 & epsilon
        sigma2[1,j]=max(omega+alpha*epsilon[1,j-1]^2+beta*sigma2[1,j-1]+gamma.v*(epsilon[2,j-1])^2, 0)
        sigma2[2,j]=max(omega.v+alpha.v*epsilon[2,j-1]^2+beta.v*sigma2[2,j-1]+gamma*(epsilon[1,j-1])^2, 0)
        epsilon[1,j]=sigma2[1,j]^0.5*eta[1,j]
        epsilon[2,j]=sigma2[2,j]^0.5*eta[2,j]
      }
      rdm01=runif(n_d*2, min=0, max=1) 
      Rdm01=matrix(rdm01,2,n_d) #(2*n_d)�H��UNI(0,1)�x�}:�Ω��H���M�w���q���^��V
      UPorDOWN=X 
      for (j in 2:n_d){
        if(Rdm01[1,j]<=prob_rise){UPorDOWN[1,j]=1}else{UPorDOWN[1,j]=-1}
        if(Rdm01[2,j]<=prob_rise.v){UPorDOWN[2,j]=1}else{UPorDOWN[2,j]=-1}
      } #(2*n_d)�x�}�A�M�w�C����q���^��V�A�W���h����=1/�U�^�h����=-1
      
      PVchg=X #(2*n_d)�x�}�A�����C��|ln(p(t+1)/p(t))|&|ln(v(t+1)/v(t))|
      PVchg[,1]=c(0,0) #���]�Ĥ@�ѻ��q���ܰ�
      for (j in 2:n_d){
        PVchg[1,j]=
          abs(max(min(rnorm(1,mean=PVchg[1,j-1],sd=sigma2[1,j-1]^0.5), 
                      logb(1.1)),logb(0.9)))*UPorDOWN[1,j]
        #ND(mean=|ln(p(t+1)/p(t))|,sd=sigma(t)) *+/-1 = ln(p(t+2)/p(t+1)�A�ܰʦb10%�H��
        if(((PVchg[1,j])>logb(1+threshold)|PVchg[1,j]<logb(1-threshold)) #���q���Y����@:�P�_���^�T�O�_�W�L���e��(5%)
           & (PVchg[1,j])*updown>0  #���q������G:�P�_������W���ΤU�^
        ){UPorDOWN[2,j]=UPorDOWN[1,j]*direction} #�ھڻ��q�����󱱨���q�P�V�ΤϦV
        PVchg[2,j]=abs(rnorm(1,mean=PVchg[2,j-1],sd=sigma2[2,j-1]^0.5))*UPorDOWN[2,j]
        #ND�H�����ͦ���q�ܤ�(�P�����k)�A�����q���^��V 
      }
      
      PV=X #(2*n_d)�x�}�A�����C����q
      PV[,1]=c(P0,V0) #������欰�u��ƾ�
      for (j in 2:n_d){
        PV[1,j]=round(PV[1,j-1]*exp(PVchg[1,j]), digits=2) #����ܤp���I��2��
        PV[2,j]=round(PV[2,j-1]*exp(PVchg[2,j])) #����q�ܭӦ��
      }
      #²��MA����
      fast=10 #�u���u10��
      slow=30 #�����u30��
      zeros=rep(0,n_d*3)
      MA=matrix(zeros,3,n_d) #(3*n_d)�x�}�A�v�C����MA(10)/MA(30)/����T�� 
      for (j in (1+fast):n_d){ #�q��11��}�l�p��e10�駡��
        tempf=0
        for (f in 1:fast){
          tempf=tempf+PV[1,j-f]
        }
        tempf=tempf/fast
        MA[1,j]=tempf
      }
      for (j in (1+slow):n_d){ #�q��31��}�l�p��e30�駡��
        temps=0
        for (s in 1:slow){
          temps=temps+PV[1,j-s]
        }
        temps=temps/slow
        MA[2,j]=temps
      }
      for (j in (1+slow):n_d){
        if(MA[1,j]>MA[2,j]){MA[3,j]=1}else{MA[3,j]=-1} #MA(10)>MA(30)�h�p��1�A�Ϥ���-1
      }
      #performence
      target_lot=30000 #�ؼжR�i�i��
      trans_fee=0.001425 #����O�βv
      fee_discount=0.5 #����O�Χ馩
      buyupper=(li+7)/200 #�]�w�R�i�W��:4%/4.5%/.../7%
      buylower=0 #�]�w�R�i�U��
      quota=runif(n_d,min=buylower,max=buyupper) #�H�����ͶR�i�W�U���������ä��t
      Trans=matrix(rep(0,n_d*4),4,n_d)
      #(4*n_d)�x�}�A����:����b��/��ѶR�X�i/��Ѥw�ֿn�X�i/�ثe�����R�J����
      
      for (j in slow:n_d){
        if(MA[3,j-1]==-1&MA[3,j]==1){ #��X�{�Ĥ@���R�i�T��(�ֽu�V�W��V�C�u)
          for (k in j:n_d){
            if((round(PV[2,k]*quota[k])+Trans[3,k-1])<=target_lot){Trans[2,k]=round(PV[2,k]*quota[k])}
            #�ֿn�i�Ƥp��ؼбi�ƫh���W���H���R�i
            else{Trans[2,k]=target_lot-Trans[3,k-1]} #�ֿn�i�Ƥ��i�W�L�ؼбi��
            Trans[3,k]=Trans[3,k-1]+Trans[2,k] #�p��֭p�i��
            Trans[1,k]=Trans[1,k-1]-(Trans[2,k]*PV[1,k]*(1+trans_fee*(1-fee_discount)))+(Trans[3,k]*PV[1,k])
            #�R�i�`��X(�t����O��)
            Trans[4,k]=(Trans[4,k-1]*Trans[3,k-1]+Trans[2,k]*PV[1,k])/(Trans[3,k]+10^-8) 
            #�p�⥭������(����+10^-8���ư�������0�����p)
          } 
        }
      }
      record=rep(0,3) #3�����V�q�A����:1st�R�i�T���X�{��Ѫѻ�/�R�i�}�l��/�R�i������
      for (j in 2:n_d){
        if(Trans[4,j-1]==0 & Trans[4,j]!=0){
          record[1]=Trans[4,j]
          record[2]=j-slow-1
        }
        if(Trans[2,j-1]!=0 & Trans[2,j]==0){
          record[3]=j-slow-1
        }
      }
      #�H�U����:�Y�ŦX�ݨD����A�~�����Ӧ��������G�A�_�h���s����
      if(record[2]>0 #���T��R�i
         & Trans[3,n_d]==target_lot #���T��bn_d�ѶR��
     
         & PV[1,50]*bullbear<PV[1,1]*bullbear 
         & PV[1,100]*bullbear<PV[1,50]*bullbear
         & PV[1,150]*bullbear<PV[1,100]*bullbear
         & PV[1,200]*bullbear<PV[1,150]*bullbear
         & PV[1,record[3]+1]*bullbear>PV[1,1]*(2^bullbear)*bullbear
         #�H�W�]�w�������C50�骺��������b�W�ɡA���R�i������j�餧���椣�W�L��1�餧2��(�ŦX�{�걡�p)�A�Ϊ�
         #�������C50�骺��������b�U�^�A���R�i������j�餧���椣�C���1�餧0.5��
        ){
        costdiff[li,n]=Trans[4,n_d]-record[1] #���������t=��������-1st�R�i�T���X�{��Ѫѻ�
        startbuyday[li,n]=record[2] #�R�i�}�l��
        endbuyday[li,n]=record[3] #�R�i������
        totalbuyday[li,n]=record[3]-record[2] #�`�R�i�Ѽ�
        lastdayprice[li,n]=PV[1,n_d] #��200�����
        print(li) #�����ɬݶ]����ӶR�i�W��
        print(n) #�����ɬݶ]��ĴX������
        print(Trans[4,n_d]) 
        print(record)
        break #�������\�A�������G���~��U������
      }
      
    }
  }
}
#�����t������
costdiffdata=data.frame("4%"=costdiff[1,],"4.5%"=costdiff[2,],"5%"=costdiff[3,],
                        "5.5%"=costdiff[4,], "6%"=costdiff[5,],"6.5%"=costdiff[6,],"7%"=costdiff[7,])
boxplot(costdiffdata,xlab = "�R�i�W��(%)",ylab = "�R�i�����t",outline=FALSE)
title("����+�����q�W") #���D
grid() #��u
#�R�i�}�l�鲰����
startbuydaydata=data.frame("4%"=startbuyday[1,],"4.5%"=startbuyday[2,],"5%"=startbuyday[3,],
                           "5.5%"=startbuyday[4,],"6%"=startbuyday[5,],"6.5%"=startbuyday[6,],"7%"=startbuyday[7,])
boxplot(startbuydaydata,xlab = "�R�i�W��(%)",ylab = "�}�l�R�i��",outline=FALSE)
title("����+�����q�W")
grid()
#�R�i�����鲰����
endbuydaydata=data.frame("4%"=endbuyday[1,],"4.5%"=endbuyday[2,],"5%"=endbuyday[3,],
                         "5.5%"=endbuyday[4,],"6%"=endbuyday[5,],"6.5%"=endbuyday[6,],"7%"=endbuyday[7,])
boxplot(endbuydaydata,xlab = "�R�i�W��(%)",ylab = "����R�i��",outline=FALSE)
title("����+�����q�W")
grid()
#�`�R�i�ѼƲ�����
totalbuydaydata=data.frame("4%"=totalbuyday[1,],"4.5%"=totalbuyday[2,],"5%"=totalbuyday[3,],
                           "5.5%"=totalbuyday[4,],"6%"=totalbuyday[5,],"6.5%"=totalbuyday[6,],"7%"=totalbuyday[7,])
boxplot(totalbuydaydata,xlab = "�R�i�W��(%)",ylab = "�`�R�i�Ѽ�",outline=FALSE)
title("����+�����q�W")
grid()
#��200��������沰����
lastdaypricedata=data.frame("4%"=lastdayprice[1,],"4.5%"=lastdayprice[2,],"5%"=lastdayprice[3,],
                            "5.5%"=lastdayprice[4,],"6%"=lastdayprice[5,],"6.5%"=lastdayprice[6,],"7%"=lastdayprice[7,])
boxplot(lastdaypricedata,xlab = "�R�i�W��(%)",ylab = "200������",outline=FALSE)
grid()

#�榸�����չ�
modeldata=data.frame(date=c(0:(n_d-slow-1)),Price=PV[1,(slow+1):n_d],Volume=PV[2,(slow+1):n_d], 
                     Fast=MA[1,(slow+1):n_d], Slow=MA[2,(slow+1):n_d], Signal=MA[3,(slow+1):n_d], 
                     AvgCost=Trans[4,(slow+1):n_d], lots_t=Trans[2,(slow+1):n_d], cum_lots=Trans[3,(slow+1):n_d])
#����+MA
p1=ggplot(data=modeldata) +
  geom_line(mapping = aes(y = Price, x = date), colour = "red", size = 1) +
  geom_line(mapping = aes(y = Fast, x = date), colour = "blue", size = 0.5) +
  geom_line(mapping = aes(y = Slow, x = date), colour = "black", size = 0.5)
#����q
p2=ggplot(data=modeldata) +
  geom_bar(mapping = aes(y = Volume, x = date), stat = "identity", size = 0.01, fill = "skyblue1")
#��������
p3=ggplot(data=modeldata) +
  geom_line(mapping = aes(y = AvgCost, x = date), colour = "black", size = 0.5)
#���R�i�i��+�ֿn�R�i�i��
p4=ggplot(data=modeldata) +
  geom_line(mapping = aes(y = cum_lots, x = date), colour = "red", size = 0.5) +
  geom_line(mapping = aes(y = lots_t, x = date), colour = "black", size = 0.5)
multiplot(p1,p2,p3,p4, cols = 2) #2*2�h��