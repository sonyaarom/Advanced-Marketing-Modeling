#1. Setting path and installing packages that are needed
path<-'/Users/sofyakonchakova/Desktop/HU Berlin/Advanced Marketing Modelling/Datasets' #change to run
setwd(path)
pacman:: p_load(readr,dplyr,ggplot2, tidyr,hrbrthemes,ggthemes)


df <- read_csv("data.iri_key.prepared.csv") #carbonated beverages data set
head(df)
colnames(df) #column names so we can understand what is in the data set
unique(df$YEAR)

#IRI KEY - encription key?
#X1 - counter
#df YEAR - 2004,2005,2006
unique(df$WEEK)
#note there are week translator in the moodle
#157 weeks for 3 years
unique(df$PRODUCT.TYPE)
#4 different product types: "SODA"          "SELTZER WATER" "CLUB SODA"     "TONIC WATER"   "BITTER LEMON" 
scent <- as.array(unique(df$FLAVOR.SCENT))
df %>%
  group_by(FLAVOR.SCENT)%>%
  summarise(total = n(), percent = total/ length(df$FLAVOR.SCENT)*100)%>%
  arrange(desc(total))%>%
  head(5)
#flavor scents - 161 different. top 5 scents are in the table, please consider that this data is for 3 years
unique(df$MARKET)
#two markets - "EAU CLAIRE" "PITTSFIELD", probably small overview about this market
unique(df$L4)
unique(df$L5) #product name
#L4 - brand name, rename for usability
names(df)[names(df) == "L4"] <- "Brand.Name"
names(df)[names(df) == "L5"] <- "Product.Name"


#vol eq check in the product description file
#packages,revenue, price is calculated there
#display and feature advertising(minor,major)
#PACKAGE "CAN"/"GLAS BOTTLE"/PLASTIC BOTTLE"

#cola as a popular flavor
sum(df$UNITS)
df%>%
  group_by(FLAVOR.SCENT)%>%
  summarize(units_sold=sum(UNITS))%>%
  mutate(share_u_s=units_sold/sum(df$UNITS)*100)%>%
  arrange(desc(share_u_s)) #43.6% of all sold units were cola scented


df%>%
  group_by(FLAVOR.SCENT)%>%
  summarize(revenue_scent=sum(DOLLARS))%>%
  mutate(share_rev=revenue_scent/sum(df$DOLLARS)*100)%>%
  arrange(desc(share_rev)) #48.6% of revenue - COLA

#filtering for cola as a flav
df_coke <-df %>%
  filter(FLAVOR.SCENT=="COLA")
unique(df_coke$PRODUCT.TYPE) #now we are left only with products of type "SODA"
#brands within filtered data set
unique(df_coke$Brand.Name) #20 different brands, let us check the biggest  money wise
unique(df_coke$Product.Name) #39 different products
df_coke %>%
  group_by(Brand.Name) %>%
  summarize(unique(Product.Name))%>%
  count()%>%
  arrange(desc(n)) #9 different products under coca cola brand, want to check year evolving

cola<-df_coke %>%
  filter(Brand.Name=="COCA COLA CO") 
cola_2004 <- cola%>%
  filter(YEAR==2004)
unique(cola_2004$Product.Name)
#biggest brands money-wise in 2004-2006: Coca Cola, Pepsico, Private label, dr pepper, adirondack
df1<- df_coke %>%
  group_by(Brand.Name, YEAR) %>%
  summarise(sales = sum(DOLLARS))%>%
  arrange(desc(sales))
df1%>%
  group_by(YEAR)%>%
  mutate(market_share=sales/sum(sales)*100)%>%
  arrange(desc(YEAR))%>%
  arrange(desc(market_share))   #market shares different years - revenue - wise
wide.df1 <- df1 %>% 
  spread(YEAR, sales)
wdf1<- as.data.frame(wide.df1)
wdf1$total = rowSums(wdf1[,2:4],na.rm=TRUE)

#top 5 brands in 2004
df_2004<- df1 %>%
  filter(YEAR == 2004)
df_2004<-as.data.frame(df_2004)
top_5_2004<-df_2004%>%
  top_n(5,sales)
#top 5 brands in 2005
df_2005<- df1 %>%
  filter(YEAR == 2005)
df_2005<-as.data.frame(df_2005)
top_5_2005<-df_2005%>%
  top_n(5,sales)
#top 5 brands in 2006
df_2006<- df1 %>%
  filter(YEAR == 2006)
df_2006<-as.data.frame(df_2006)
top_5_2006<-df_2006%>%
  top_n(5,sales)
#top 5 brands in all years
df_2<-rbind(top_5_2004,top_5_2005)
df_2<-rbind(df_2,top_5_2006)
top_5 <- df_2
#small check if there are only 5 unique brands every year
top_brand<-unique(top_5$Brand.Name)

top<-df_coke %>%
  filter(Brand.Name %in% top_brand)
calendar <- readxl::read_xlsx("IRI week translation.xlsx") 
calendar <- as.data.frame(calendar)
head(calendar)
calendar<-calendar[,-c(4:6)]
head(calendar)
names(calendar)[names(calendar) == "IRI Week...1" ] <- "WEEK"
names(calendar)[names(calendar) == "Calendar week starting on" ] <- "W.Start"
names(calendar)[names(calendar) == "Calendar week ending on" ] <- "W.End"
calendar$W.Start <- as.Date(calendar$W.Start,format = "%Y-%m-%d")
calendar$W.End <- as.Date(calendar$W.End,format = "%Y-%m-%d")
calendar<-calendar %>%
  mutate(month = format(W.Start,"%m"))
top<-top%>%
 dplyr::inner_join(calendar, by="WEEK")

df_coke <- df_coke%>%
  dplyr::inner_join(calendar, by="WEEK")

df <- df %>%
  dplyr::inner_join(calendar, by="WEEK")

ggthemes::
Monthly_Rev <- df_coke%>%
  group_by(YEAR,month)%>%
  summarize(sales=sum(DOLLARS))%>%
  arrange(desc(sales))%>%
  ggplot(aes(month,sales/1000))+
  geom_point()+
  geom_line(aes(group=1),alpha=0.5)+
  scale_y_continuous(name="Sales, K$")+
  xlab("Month")+
  facet_wrap(~YEAR,dir="v")+
  theme_hc()

colnames(df_coke)



df_coke%>%
  group_by(YEAR,month)%>%
  summarize(Volume=sum(VOL_EQ))%>%
  ggplot(aes(month,Volume))+
  geom_point()+
  geom_line(aes(group=1),alpha=0.5)+
  facet_wrap(~YEAR,dir="v")+
  xlab("Month")

#market share overall by volume sold
volume_share<-df_coke%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(VOL_EQ))%>%
  arrange(desc(sales))

sum(volume_share$sales) #60652.27
volume_share<-volume_share%>%
  mutate(MS = (sales/60652.27)*100) #87% of all volume is on pepsico and coca cola
#volume 2004
volume_share_2004<-df_coke%>%
  filter(YEAR==2004)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(VOL_EQ))%>%
  arrange(desc(sales))

sum(volume_share_2004$sales) #20170.59
volume_share_2004<-volume_share_2004%>%
  mutate(MS_2004 = (sales/20170.59)*100)
#volume 2005
volume_share_2005<-df_coke%>%
  filter(YEAR==2005)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(VOL_EQ))%>%
  arrange(desc(sales))

sum(volume_share_2005$sales) #20287.54
volume_share_2005<-volume_share_2005%>%
  mutate(MS_2005 = (sales/20287.54)*100)
#volume 2006
volume_share_2006<-df_coke%>%
  filter(YEAR==2006)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(VOL_EQ))%>%
  arrange(desc(sales))

sum(volume_share_2006$sales) #20194.15
volume_share_2006<-volume_share_2006%>%
  mutate(MS_2006 = (sales/20194.15)*100)
volume_share_2004 <- as.data.frame(volume_share_2004)
volume_share_2005 <- as.data.frame(volume_share_2005)
volume_share_2006<- as.data.frame(volume_share_2006)
vol_sh <- inner_join(volume_share_2004,volume_share_2005,by="Brand.Name")
vol_sh<- inner_join(vol_sh,volume_share_2006,by="Brand.Name")
vol_sh <- vol_sh[,c(1,3,5,7)]
"Cola took over Pepsi volumes, becoming market leader by volumes sold, we can zoom in into quarter 
dynamics to check at what moment it changed + have a look if they extensively used promotions techniques"
#market share overall by units sold
units_share<-df_coke%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(UNITS)) %>%
  arrange(desc(sales))
sum(units_share$sales) #5628595
units_share<-units_share%>%
  mutate(MS = (sales/5628595)*100) #96% of all sold units are on pepsico and coca cola

#units 2004
units_MS_2004 <- df_coke%>%
  filter(YEAR==2004)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(UNITS)) %>%
  arrange(desc(sales))

sum(units_MS_2004$sales) #1863991

units_MS_2004 <- units_MS_2004%>%
  mutate(MS_2004=as.numeric(round(sales/1863991*100,2)))

#units 2005
units_MS_2005 <- df_coke%>%
  filter(YEAR==2005)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(UNITS)) %>%
  arrange(desc(sales))
sum(units_MS_2005$sales) #1934328

units_MS_2005 <- units_MS_2005%>%
  mutate(MS_2005=as.numeric(round(sales/1934328*100,2)))


#units 2006
units_MS_2006 <- df_coke%>%
  filter(YEAR==2006)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(UNITS)) %>%
  arrange(desc(sales))
sum(units_MS_2006$sales) #1830276
units_MS_2006 <- units_MS_2006 %>%
  mutate(MS_2006=as.numeric(round(sales/1830276*100,2)))
         
#aggregating the output together
units_MS_2004 <- as.data.frame(units_MS_2004)
units_MS_2005 <- as.data.frame(units_MS_2005)
units_MS_2006<- as.data.frame(units_MS_2006)

un_sh <- inner_join(units_MS_2004,units_MS_2005,by="Brand.Name")
un_sh<- inner_join(un_sh,units_MS_2006,by="Brand.Name")
un_sh <- un_sh[,c(1,3,5,7)]
un_sh
#market share by revenue
rev_sharee<-df_coke%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(DOLLARS)) %>%
  arrange(desc(sales))

sum(rev_sharee$sales) #13815042

rev_sharee <- rev_sharee%>%
  mutate(MS = as.numeric(sales/13815042*100))
write_csv2(head(rev_sharee,3),"revenue.csv")
#revenues 2004

rev_sharee_2004<-df_coke%>%
  filter(YEAR == 2004)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(DOLLARS)) %>%
  arrange(desc(sales))

sum(rev_sharee_2004$sales) #4685233

rev_sharee_2004 <- rev_sharee_2004%>%
  mutate(MS_2004 = as.numeric(round(sales/4685233*100),2))

#revenues 2005

rev_sharee_2005<-df_coke%>%
  filter(YEAR == 2005)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(DOLLARS)) %>%
  arrange(desc(sales))

sum(rev_sharee_2005$sales) #4630344

rev_sharee_2005 <- rev_sharee_2005%>%
  mutate(MS_2005 = as.numeric(round(sales/4630344*100),2))

#revenues 2006

rev_sharee_2006<-df_coke%>%
  filter(YEAR == 2006)%>%
  group_by(Brand.Name)%>%
  summarize(sales=sum(DOLLARS)) %>%
  arrange(desc(sales))

sum(rev_sharee_2006$sales) #4499466

rev_sharee_2006 <- rev_sharee_2006%>%
  mutate(MS_2006 = as.numeric(round(sales/4499466*100),2))

#bringing it together
rev_sharee_2004 <- as.data.frame(rev_sharee_2004)
rev_sharee_2005 <- as.data.frame(rev_sharee_2005)
rev_sharee_2006<- as.data.frame(rev_sharee_2006)

rev_sh <- inner_join(rev_sharee_2004,rev_sharee_2005,by="Brand.Name")
rev_sh<- inner_join(rev_sh,rev_sharee_2006,by="Brand.Name")
rev_sh <- rev_sh[,-c(2,4,6)]
rev_sh<- head(rev_sh,5)
#write_csv2(head(vol_sh,5),"Volume Sold Share.csv")
#write_csv2(head(un_sh,5), "Units Sold Share.csv")
#write_csv2(rev_sh, "Revenue Share.csv")

head(rev_sharee)
head(units_share)
head(volume_share)

unique(df_coke$month)
df_coke$month <- as.numeric(df_coke$month)

#let us create quarters
df_quarters <- data.frame(month=c(1:12),
                          quarter=rep(c(1:4),each=3))
df_quarters

df_coke_q <- df_coke%>%
  merge(df_quarters,by="month")

head(df_coke)
unique(df_coke$month)
Pep_cola<-df_coke_q%>%
  filter(Brand.Name %in% c("COCA COLA CO","PEPSICO INC"))
#CAN BE USED ONLY IF WE CONSIDER Pepsi+Cola = 100%
p_c1<- Pep_cola %>%
  group_by(Brand.Name, YEAR, quarter)%>%
  summarize(q_sales=sum(VOL_EQ))

p_c1<-p_c1 %>%
  group_by(YEAR,quarter)%>%
  mutate(total=sum(q_sales))%>%
  arrange(quarter)%>%
  mutate(share=q_sales/total*100)

ggplot(p_c1, aes(quarter,share,color=Brand.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR, ncol=1)+
  theme_gray()+
  ylab("Share of the volume sold")

#otherwise
pc2<- df_coke_q %>%
  group_by(Brand.Name, YEAR, quarter)%>%
  summarize(q_sales=sum(VOL_EQ))

pc_4<-df_coke_q %>%
  group_by(Brand.Name, YEAR, quarter)%>%
  summarize(q_sales=sum(UNITS))%>%
  group_by(YEAR,quarter)%>%
  mutate(total=sum(q_sales))%>%
  arrange(quarter)%>%
  mutate(share=round(q_sales/total*100,2))%>%
  filter(Brand.Name %in% c("COCA COLA CO","PEPSICO INC"))

pc_4$total


pc2<-pc2 %>%
  group_by(YEAR,quarter)%>%
  mutate(total=sum(q_sales))%>%
  arrange(quarter)%>%
  mutate(share=round(q_sales/total*100,2))%>%
  filter(Brand.Name %in% c("COCA COLA CO","PEPSICO INC"))



jpeg("my_plot.jpeg",units="in", width=7, height=6, res=300)

ggplot(pc2, aes(quarter,share,color=Brand.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR, ncol=1)+
  ylab("Volume Market Share,%")+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  xlab("Quarters")+
  theme(legend.position = "bottom")+
  labs(color="Brand Name:")
dev.off()
#names <- list("DIET RITE", "CAFFEINE FREE DIET PEPSI", "DIET COKE", "DIET PEPSI","DIET COKE WITH SPLENDA", "DIET R C","DIET COKE WITH SPLENDA","COKE ZERO","PEPSI ONE"            )


pc3 <- df_coke_q %>%
  group_by(YEAR, quarter)%>%
  summarize(sales=sum(VOL_EQ))

pc3$YEAR <- as.factor(pc3$YEAR)
jpeg("test.jpeg", units="in", width=8, height=5, res=300)

ggplot(pc3,aes(quarter,sales*192*0.0296/1000,color=YEAR))+
  geom_point()+
  geom_line()+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  xlab("Quarters")+
  ylab("Sold Volumes, 1000L")+
  labs(color="Year")+
  ylim(c(25,32))+
  theme(legend.position = "bottom")
dev.off()
plot2

dev.off()
unique(df_coke$VOL_EQ)

df_x<-df_coke_q %>%
  filter(Brand.Name %in% c("COCA COLA CO","PEPSICO INC"))%>%
  group_by(Brand.Name, Product.Name,YEAR,quarter)

head(df_x)
df_xy<-df_x %>%
  filter(Brand.Name=="PEPSICO INC") %>%
  group_by(YEAR, quarter, Product.Name)%>%
  summarize(units_sold = sum(UNITS))

dev.off()
df_xx<-df_x %>%
  filter(Brand.Name=="COCA COLA CO") %>%
  group_by(YEAR, quarter, Product.Name)%>%
  summarize(units_sold = sum(UNITS))

ggplot(df_xx,aes(quarter,units_sold,color=Product.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR,ncol=1)

df_z<-df_x %>%
  group_by(Brand.Name,Product.Name, YEAR)%>%
  summarize(Revenue = sum(DOLLARS))%>%
  group_by(YEAR)


df_z<-df_z%>%
  group_by(YEAR, Brand.Name)%>%
  mutate(total = sum(Revenue))%>%
  mutate(Share=Revenue/total)
df1
df_z<-df_z%>%
  mutate(PRODUCT = ifelse(Share<0.14,"OTHER",Product.Name))%>%
  arrange(desc(Share))

df_z<-df_z%>%
  mutate(PRODUCT = ifelse(Share<0.10,"OTHER",Product.Name))%>%
  group_by(YEAR,Brand.Name,PRODUCT)%>%
  summarize(Share_x=sum(Share))

  
jpeg("plot_brands.jpeg", units="in", width=8, height=5, res=1000)

ggplot(df_z,aes(x="", y=Share_x, fill=PRODUCT)) +
  geom_bar(stat="identity") +
  coord_polar(theta="y")+
  scale_fill_brewer(palette="Pastel1")+
  facet_wrap(~Brand.Name+YEAR)+
  theme_void()+
  theme(legend.position = "bottom", text=element_text(family = "Times New Roman",size=12,color="black"),plot.margin = margin(c(10,1,10,1),))+
  geom_text(aes(label = paste0(round(Share_x*100), "%")),family = "Times New Roman",size=4,position=position_stack(vjust=0.6),color="black",angle=45)
  
dev.off()






packages<-df_coke_q%>%
  filter(Brand.Name=="COCA COLA CO")%>%
  group_by(YEAR, quarter,PACKAGE)%>%
  summarise(SOLD=sum(UNITS))
dev.off()
packages_q_y<-packages%>%
  group_by(YEAR,quarter)%>%
  mutate(TOTAL=sum(SOLD))%>%
  mutate(SHARE=SOLD/TOTAL*100)%>%
  ggplot(aes(quarter,SHARE,color=PACKAGE))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR,ncol=1)
tail(packages_q_y,10)

head(df_coke)

tmp<-aggregate(cbind(UNITS, DOLLARS) ~  Brand.Name+Product.Name+VOL_EQ+PACKAGE+YEAR, 
               data = df_coke, sum, na.rm = TRUE)

tmp_unit<-tmp%>%
  group_by(YEAR,PACKAGE)%>%
  summarize(UNITS=sum(UNITS))

tmp_unit <- tmp_unit%>%
  group_by(YEAR)%>%
  mutate(TOTAL=sum(UNITS))%>%
  mutate(SHARE=UNITS/TOTAL*100)%>%
  spread(YEAR,SHARE)

write_csv2(tmp_unit, "Package_shares.csv")

Mean_Price<-tmp%>%
  mutate(PRICE=DOLLARS/UNITS)%>%
  group_by(PACKAGE,YEAR)%>%
  summarize(Mean_Price = mean(PRICE))

Mean_Price_wide <- Mean_Price%>%
  spread(YEAR,Mean_Price)
#write_csv2(Mean_Price_wide,"Mean_price.csv")
#Checking Glass bottle sales for Coca Cola Co
glass<-df_coke%>%
  filter(PACKAGE=="GLAS BOTTLE")%>%
  filter(Brand.Name=="COCA COLA CO")
total<-glass%>%
  group_by(YEAR, month)%>%
  summarize(TOTAL=sum(UNITS))
total <- as.data.frame(total)

#jpeg("total.jpeg", units="in", width=8, height=5, res=1000)
#The Total Unit Sold graph
total%>%
  ggplot(aes(as.factor(month), TOTAL,group=as.factor(YEAR), color=as.factor(YEAR)))+
  geom_point()+
  geom_line()+
  coord_fixed(ratio=0.01)+
  ylab("Total Units Sold")+
  xlab('Month')+
  theme(legend.position = "bottom",  text=element_text(family="Times New Roman",size=10))+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  labs(color="Year:")
#dev.off()




main <- df_coke%>%
  filter(Brand.Name %in% c("COCA COLA CO", "PEPSICO INC"))%>%
  group_by(Brand.Name,WEEK,feature_all,Product.Name)%>%
  summarize(TOTAL=sum(UNITS))

ggplot(main, aes(WEEK,TOTAL,group=as.factor(feature_all),color=as.factor(feature_all)))+
  geom_point()+
  geom_line()+
  facet_wrap(~Brand.Name)

xtabs(display_all~Brand.Name, main)


unique(df$feature_medium)
write.csv2(df_coke_q,"df_coke_quarters.csv")

#df_coke_clean <- df_coke%>%
  mutate(feature_all=ifelse(feature_all %in% c(0,1),feature_all,NA))
unique(df_coke_clean$feature_all)
