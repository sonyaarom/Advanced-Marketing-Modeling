#Cleaner code for the promotion analysis
path<-'/Users/sofyakonchakova/Desktop/HU Berlin/Advanced Marketing Modelling/Datasets' #change to run
setwd(path)
pacman:: p_load(readr,dplyr,ggplot2, tidyr,hrbrthemes,ggthemes,ggcorrplot,RColorBrewer)

#The data set was prepared beforehand, save it to the folder with the previous datasets
df_coke_q <- read_csv2("df_coke_quarters.csv") 
dim(df_coke_q)
colnames(df_coke_q) #110307     26 check so i am not deleting to much


unique(df_coke_q$display_all) #a lot of weird values
unique(df_coke_q$feature_all)
#there are different weird values in the datasets in the columns that were supposed to be dummycoded
#i will delete columns when dummy code is broken
df_coke_clean <- df_coke_q%>%
  mutate(feature_all=ifelse(feature_all %in% c(0,1),feature_all,10))

table(df_coke_clean$feature_all) #56 wrong values, 4 is just randomly assigned 
df_coke_clean <- subset(df_coke_clean, df_coke_clean$feature_all %in% c(0,1))


unique(df_coke_clean$feature_all) #0,1 all wrong deleted, then I will check other columns
unique(df_coke_clean$feature_medium) #checked other columns, everything is right, will do the same with other columns

#doing the same for the display rows
df_coke_clean <- df_coke_clean%>%
  mutate(display_all=ifelse(display_all %in% c(0,1),display_all,4))
table(df_coke_clean$display_all) #800 wrong lines
df_coke_clean <- subset(df_coke_clean, display_all %in% c(0,1))

df_coke_clean <- df_coke_clean%>%
  mutate(display_major=ifelse(display_major %in% c(0,1),display_major,4))
table(df_coke_clean$display_major) #26 lines to delete
df_coke_clean <- subset(df_coke_clean, display_major %in% c(0,1))

dim(df_coke_clean) #109425     26

#preparing data columns to be represented in a better way 
df_coke_clean$quarter <- as.factor(df_coke_clean$quarter)
df_coke_clean$WEEK <- as.factor(df_coke_clean$WEEK)
df_coke_clean$YEAR <- as.factor(df_coke_clean$YEAR)


#overall used by these 11 brands
display<-df_coke_clean%>%
  filter(display_all==1)
unique(display$Brand.Name) 


#shares of all yearly display used
plt1<-display%>%
  group_by(Brand.Name,YEAR,month)%>%
  mutate(display_used=sum(display_all))%>%
  group_by(Brand.Name,YEAR)%>%
  summarize(total_year=sum(display_used))
plt1%>%
  arrange(desc(total_year))%>%
  group_by(YEAR)%>%
  mutate(total=sum(total_year))%>%
  mutate(share=total_year/total*100)


#display by both brands
un<- df_coke_clean%>%
  filter(Brand.Name=="PEPSICO INC")%>%
  group_by(YEAR,month,Product.Name)%>%
  summarize(SUM=sum(display_all))

un<-as.data.frame(un)


#jpeg("RPlot_Display_Pepsi.jpeg",res=300)
ggplot(un, aes(as.factor(month),SUM,group=Product.Name,color=Product.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR,ncol=1)+
  ylab("Times Display Ads Were Used")+
  xlab('Month')+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  labs(color="Product:")+
  theme(legend.position = "bottom",  text=element_text(family="Times New Roman",size=10))+
  coord_fixed(ratio=0.009)
#dev.off()

ux<- df_coke_clean%>%
  filter(Brand.Name=="PEPSICO INC")%>%
  group_by(YEAR,month,PACKAGE)%>%
  summarize(SUM=sum(display_all))

ggplot(ux, aes(as.factor(month),SUM,group=PACKAGE,color=PACKAGE))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR,ncol=1)


#display - PepsiCo Inc, Coca Cola Co
uf<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name)%>%
  summarize(SUM=sum(display_all))

#jpeg("RPlot_Display_Both.jpeg", units="in", width=8, height=5, res=1000)
ggplot(uf, aes(as.factor(month),SUM,group=Brand.Name,color=Brand.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR,ncol=1)+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  labs(color="Brand:")+
  theme(legend.position = "bottom",  text=element_text(family="Times New Roman",size=10))+
  xlab("Month")+
  ylab("Total Times Display Ads Used")
#dev.off()
#to see what brands used display ads, and if yes, in how many weeks in different years. Answer: not all brands, all weeks 
uma<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name)%>%
  summarize(Major=sum(display_major))
umi<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name)%>%
  summarize(Minor=sum(display_minor))
uma <- as.data.frame(uma)
umi <- as.data.frame(umi)

mami <- merge(uma,umi)

mami_long <- mami%>% 
  gather(Type,Times, -c(YEAR,month,Brand.Name))
head(mami_long)
mami%>%
  group_by(YEAR,Brand.Name)%>%
  summarize(mean(Major))
mami%>%
  group_by(YEAR,Brand.Name)%>%
  summarize(mean(Minor))
#jpeg("RPlot_Display_Both_Display.jpeg", units="in", width=12, height=8, res=1000)

ggplot(mami_long, aes(as.factor(month), Times, group=Brand.Name, color=Brand.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR+Type,ncol=2)+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  theme(legend.position="bottom")+
  labs(color="Brand:")+
  xlab("Month")+
  ylab("Total Times Used")+
  scale_y_continuous()+
  coord_fixed(ratio=0.0075)
#dev.off()
#zoom into product names and display ads

umi_p<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name,Product.Name)%>%
  summarize(Minor=sum(display_minor))

uma_p <- as.data.frame(uma_p)
umi_p <- as.data.frame(umi_p)

mami_p <- merge(uma_p,umi_p)
mami_long_p <- mami_p%>% 
  gather(Type,Times, -c(YEAR,month,Brand.Name,Product.Name))

unique(mami_p$Product.Name)



n <- 15
col_vector <- c("#ff8318",
                "#4b98ff",
                "#db1100",
                "#00a0a9",
                "#cba5cc",
                "#576a00",
                "#9d5aff",
                "#005f3d",
                "#ff4b87",
                "#00428e",
                "#fabb5c",
                "#980079",
                "#7a4300",
                "#ffaf96",
                "#563f26")
pie(rep(1,n), col=sample(col_vector, n)) #color check


#jpeg("RPlot_Display_Product_Display.jpeg", units="in", width=13, height=8, res=1000)
p<-ggplot(mami_long_p, aes(as.factor(month), Times, group=Product.Name, color=Product.Name))+
  geom_point()+
  geom_line()+
  facet_grid(Brand.Name~Type+YEAR)+
  theme(legend.position="bottom")+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  theme(legend.position="bottom")+
  xlab("Month")+
  ylab("Times")

p+scale_color_manual(values = col_vector,name="Product:")
#dev.off()
#add most frequently used marketing activities by brand and period

unique(df_coke_q$Brand.Name)

df_coke_q%>%
  filter(Brand.Name=="COCA COLA CO", Product.Name=="COKE ZERO")%>%
  group_by(YEAR, month)%>%
  summarize(sum=sum(UNITS))


#FEATURE ADVERTISING - DOING ABSOLUTELY THE SAME
#overall used by these 11 brands
display<-df_coke_clean%>%
  filter(feature_all==1)
unique(display$Brand.Name) #overall used by these 8 brands
#feature_all - PepsiCo Inc, Coca Cola Co

ud<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name)%>%
  summarize(SUM_Feature=sum(feature_all))


ggplot(ud, aes(as.factor(month),SUM_Feature,group=Brand.Name,color=Brand.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~YEAR,ncol=1)+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  labs(color="Brand:")+
  theme(legend.position = "bottom",  text=element_text(family="Times New Roman",size=10))+
  xlab("Month")+
  ylab("Total Times Feature Ads Used")

#will put overall feature and display together

ud <- as.data.frame(ud)
uf<- as.data.frame(uf)
promo <- merge(ud,uf)
names(promo)[names(promo)=="SUM_Feature"]<- 'Feature'
names(promo)[names(promo)=="SUM"]<- 'Display'
head(promo)
promo <- promo%>%
  gather(Promotion, Total, -c("YEAR","month","Brand.Name"))


#jpeg("Brand_Promo.jpeg", units="in", width=13, height=8, res=1000)
ggplot(promo,aes(as.factor(month),Total,group=Brand.Name,color=Brand.Name))+
  geom_point()+
  geom_line()+
  facet_grid(YEAR~Promotion)+
  theme(legend.position="bottom")+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  theme(legend.position="bottom")+
  xlab("Month")+
  ylab("Times")+
  labs(color="Brand:")

#dev.off()


#Corrplot
head(df_coke_clean)
colnames(df_coke_clean)
corr<- df_coke_clean[,c("VOL_EQ","UNITS","price" ,"display_all","feature_all" )]
corr
model.matrix(~0+., data=corr) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#Feature major-minor-medium
fsm<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name,Product.Name)%>%
  summarize(Small=sum(feature_small))
fme<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name,Product.Name)%>%
  summarize(Medium=sum(feature_medium))
fla<- df_coke_clean%>%
  filter(Brand.Name %in% c("PEPSICO INC","COCA COLA CO"))%>%
  group_by(YEAR,month,Brand.Name,Product.Name)%>%
  summarize(Large=sum(feature_large))


fsm <- as.data.frame(fsm)
fme <- as.data.frame(fme)
fla <- as.data.frame(fla)


display <-merge(fsm,fme)
display <-merge(display,fla)
display_long <- display%>% 
  gather(Type,Times, -c(YEAR,month,Brand.Name,Product.Name))

display_long <- subset(display_long,display_long$Times!=0)
vector<-unique(display_long$Product.Name)
vector <- sort(vector)
sort(unique(display$Product.Name)) # COKE was never promoted with feature advertising

t<-ggplot(display_long, aes(as.factor(month), Times, group=Product.Name, color=Product.Name))+
  geom_point()+
  geom_line()+
  facet_grid(Brand.Name+YEAR~Type)+
  theme(legend.position="bottom")+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  theme(legend.position="bottom")+
  xlab("Month")+
  ylab("Times")
#jpeg("Feature.jpeg", units="in", width=13, height=8, res=1000)
t+scale_color_manual(values = col_vector,name="Product:")

# Seasonality of sales for Cola flavored beverages
ses<-df_coke_clean%>%
  group_by(YEAR, month)%>%
  summarise(units=sum(UNITS))
ses<-ses%>%
  group_by(YEAR)%>%
  mutate(Total=sum(units))%>%
  mutate(Share=units/Total*100)
# jpeg("Sales Seasonality.jpeg", units="in", width=13, height=8, res=1000)

ses %>%
  ggplot(aes(as.factor(month), Share,group=YEAR, color=YEAR))+
  geom_point()+
  geom_line()+
  theme(legend.position="bottom")+
  theme_ipsum(axis_title_just="cc",base_family = "Times New Roman", base_size = 12,axis_text_size = 10,subtitle_size = 10, caption_size = 10,axis_title_size = 12, axis_title_face = "plain",strip_text_face = "bold",strip_text_size = 11)+
  theme(legend.position="bottom")+
  xlab("Month")+
  ylab("Share of sold units,%")+
  coord_fixed(ratio=1)

#dev.off()

display_long %>%
  filter(Type=="Small", Brand.Name =="PEPSICO INC")%>%
  group_by(Product.Name)%>%
  summarize(sum(Times))
