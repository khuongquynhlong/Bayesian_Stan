rm(list = ls())
library(tidyverse)

df = read.csv("ICU.csv", header = T) %>% as_tibble()
head(df)
###############################################################################
# Data co ten la ICU, day la nghien cuu cat ngang khao sat huyet ap va mach
# tren 200 benh nhan shock nhiem khuan trong khoa ICU. Cau hoi nghien cuu la
# So sanh huyet ap tam thu (SysBP) giua 2 nhom benh nhan Tu vong va Song sot.

####
# Add Outcome variable
df$Outcome<-recode_factor(df$Survive,
                          `0`="Dead",
                          `1`="Survived")
df$Survive<-recode_factor(df$Survive,
                          `0`="1",
                          `1`="2")
psych::describeBy(df$SysBP, group = df$Outcome)

# Histogram cua bien SysBP theo Outcome
df%>%ggplot(aes(x=SysBP,fill=Outcome))+
    geom_density(alpha=.6,col="black")+
    geom_histogram(aes(y=..density..,col=Outcome),alpha=0.5)+
    facet_wrap(~Outcome,ncol=1,scales="free_y")+
    theme_bw()

####
# t-test Frequentist
var.test(df$SysBP ~ df$Outcome)
t.test(df$SysBP ~ df$Outcome)

#  tinh Effect size
effsize::cohen.d(df$SysBP, df$Outcome)

#############################################################################
### stan code
library(rstan)

stanmodelcode ="
data {
int<lower=1> N;      // Co mau (lower=1: de kiem tra null data ?)
int<lower=2> Ng;    // So phan nhom (lower =2: it nhat phai co 2 phan nhom)
real<lower=5> n1;   // Co mau phan nhom 1: it nhat phai co 5 cases
real<lower=5> n2;   // Co mau phan nhom 2: it nhat phai co 5 cases
vector[N] y;         // Bien ket qua Y la 1 vector co do dai=N
int<lower=1, upper=2> groupID[N];   //  group ID la 1 so nguyen nhan 2 gia tri =1 hoac =2
}

transformed data{
real meany;      // Bien meany la trung binh cua Y la 1 so thuc; prior cua no la mu
meany = mean(y); // Xac dinh gia tri meany= ham mean
}

parameters {
vector[2] mu;                // Khai bao tham so cua phan phoi Student-t: mu, sigma va nu; mu la 1 vector 2 gia tri
vector<lower=0>[2] sigma;   // sigma la 1 vector, gioi han duoi =0, nhan 2 gia tri
real<lower=0, upper=100> nu; // nu la do tu do hay kieu hinh cua phan phoi t, gioi han 0:100
}

transformed parameters {   // Phan hoan chuyen tham so cua mo hinh stan: khong can thiet
}

model {                       // Khai bao noi dung mo hinh 
// khai bao tien dinh (prior)
mu ~ normal(meany, 10);         // Tien dinh (prior) cua tham so mu la phan phoi Gaussian, tb=meany, sd=10
sigma ~ cauchy(0, 5);           // Tien dinh cua tham so Sigma la pp Cauchy(0,5)
nu ~ exponential(1.0/29);        // Tien dinh cua tham so Nu la pp Exponential voi rate=29


// khai bao likelihood = 1 vong lap

for (n in 1:N){
y[n] ~ student_t(nu, mu[groupID[n]], sigma[groupID[n]]);
}

}

generated quantities {    // Tinh toan cac tham so trong t_test 
vector[N] Res;           // Phan phoi hau dinh cua phan vi gia tri du bao Y theo mo hinh, yRep la 1 vector do dai N
real muDiff;            // Khac biet trung binh : muDiff la 1 so thuc
real s;                // pooled sd
real Cohensd;         // Cohen's d effect size (Jacob Cohen) la 1 so thuc
real HedgesG;        // Hedges's G effect size ( Larry Hedges, 1981), la so thuc

for (n in 1:N){
Res[n] = student_t_rng(nu, mu[groupID[n]], sigma[groupID[n]]);
}

muDiff = mu[2] - mu[1];
s=sqrt(((n1-1)*sigma[1]^2+(n2-1)*sigma[2]^2)/(n1+n2-2)); //Tinh pooled sd
Cohensd = muDiff / s;                                    // Tinh Cohen's d
HedgesG = (muDiff / s)*(1- (3/(4*(n1+n2)-9)));                                    // Tinh Hedges's G
}
"
############################################################################
# plot Prior
p1=rexp(5000,1/29)%>%
    as.data.frame()%>%
    ggplot(aes(x=.))+
    geom_density(alpha=0.5,col="black",fill="red")+
    theme_bw()+ggtitle("Prior cho nu")

p2=rnorm(5000,mean(df$SysBP),10)%>%
    as.data.frame()%>%
    ggplot(aes(x=.))+
    geom_density(alpha=0.5,col="black",fill="red")+
    theme_bw()+ggtitle("Prior cho Mu")

p3=rcauchy(5000,location = 0,scale =5)%>%
    as.data.frame()%>%
    ggplot(aes(x=.))+xlim(c(0,max(df$SysBP)))+
    geom_density(alpha=0.5,col="black",fill="red")+
    theme_bw()+ggtitle("Prior cho Sigma")

p4=df%>%ggplot(aes(x=SysBP))+
    geom_density(alpha=0.5,col="black",fill="red")+
    theme_bw()+ggtitle("Phan phoi thuc te cua Y")

gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2)
############################################################################


