#this code is used for energy audit of the plant based on production and power consumption

dataproduction = dbConnect(MySQL(), user='root', password='', dbname='maynilad_ar1', host='localhost')
dbListTables(dataproduction)
dbListFields(dataproduction, 'analysisdate')

rs = dbSendQuery(dataproduction, "select * from analysisdate")

rs=dbFetch(rs, n = -1)

#rs=dbReadTable(pagcordata, 'analysisdate')

#rs=c(rs[2],rs[3],rs[4],rs[5])

data=c(rs[1],rs[4],rs[5],rs[6])

#data <- read.csv(file="pag_analysisdate.csv", header=TRUE, sep=",")

a=as.Date(data$date,"%Y-%m-%d")
production=data$total_pro_hour
power=data$total_power_hour
ratio=data$ratio


#data$date=format.Date(data$date, format="%Y-%b-%d")

data=data.frame(a,production,power,ratio)

#print(data)


#Correlation analysis


hydropairs(data[,2:3])


#Plot production data
plot.new()
p=ggplot(data=data,aes(a, production))+ geom_line(color = "#00AFBB") + theme(axis.text.x = element_text(angle = 60,hjust=1))+theme_gray(base_size = 14)+ theme(plot.title = element_text(size=12))
#p
min <- as.Date("2017-01-01")
max <-  as.Date("2018-8-31")
#p + scale_x_date(limits = c(min, max))
pp=p+scale_x_date(date_labels = "%Y",date_breaks="year",limits = c(min, max))+labs(title = "Production hourly",x="Years",y="ML")+scale_y_continuous(limits=c(0,10))#+ stat_smooth(method="lm")

print(pp)
#box()



#Plot power consumption data
plot.new()
q=ggplot(data=data,aes(a, power))+ geom_line(color = "#00AFBB") + theme(axis.text.x = element_text(angle = 60,hjust=1))+theme_gray(base_size = 14)+ theme(plot.title = element_text(size=12))
#q
min <- as.Date("2017-01-01")
max <-  as.Date("2018-8-31")
#p + scale_x_date(limits = c(min, max))
qq=q+scale_x_date(date_labels = "%Y",date_breaks="year",limits = c(min, max))+labs(title = "Power hourly",x="Years",y="KW")+scale_y_continuous(limits=c(0,1500))#+ stat_smooth(method="lm")

print(qq)
#plot ratio

plot.new()
w=ggplot(data=data,aes(a, ratio))+ geom_line(color = "#00AFBB") + theme(axis.text.x = element_text(angle = 60,hjust=1))+theme_gray(base_size = 14)+ theme(plot.title = element_text(size=12))
#w
min <- as.Date("2017-01-01")
max <-  as.Date("2018-8-31")
#p + scale_x_date(limits = c(min, max))
ww=w+scale_x_date(date_labels = "%Y",date_breaks="year",limits = c(min, max))+labs(title = "Ratio",x="Years",y="")+scale_y_continuous(limits=c(0,450))#+ stat_smooth(method="lm")

print(ww)

#stop()

dbDisconnect(dataproduction) 
