setwd("/home/katieyth/")

##########library---------------------------------------------
library(ggplot2)
library(gsheet)
library(data.table)
library(ggpubr)#ggtexttable
library(scales)#pretty break
library(png)
library(grid) # rasterGrob
library(gtools) # odd
#library(googlesheets)


##########theme-------------------------------------------
theme_set(theme_bw()+
            theme(plot.title = element_text(size = 25),
                  plot.subtitle = element_text(size = 18),
                  axis.text.x = element_text(size = 18),
                  axis.text.y = element_text(size = 18),
                  axis.title.x = element_text(size = 22),
                  axis.title.y = element_text(size = 22),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = 'top',
                  #legend.text = element_text(size = 18),
                  #legend.title = element_text(size = 20),
                  strip.background = element_rect(fill = 'papayawhip'),
                  strip.text = element_text(size = 20)))
##########function--------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###---Pre.SET---------------------------------------------
list.gsheet <- c("https://.....")
bar.big <- 8
bar.small <- 6
text.time <- 8
list.class.name <- c("Tech", "Manager")
ch.class.names <- c("[技術領袖培訓班] - 第一期" ,"[經理人週末研修班] - 第一期")

###---which.class-------------------------------
which.class <- 1  #1:aia, 2, aia.w
class.name <- list.class.name[which.class]
class.ch.name <- ch.class.names[which.class]
###---renamecol and createcol-------------------
aia.data <- gsheet2text(list.gsheet[which.class])
aia.data <- fread(aia.data, stringsAsFactors=FALSE, encoding = "UTF-8")
colnames(aia.data) <- c("Timestamp", "Email", "name",
                        "nation", "gender", "birthday",
                        "army", "ID", "passport",
                        "address", "cellphone", "email2", 
                        "photo", "firm.department", "occupation",
                        "web", "LinkedIn", "github",
                        "highest.edu", "highest.edu.doc", "bachelor.score",
                        "post.deg", "master.deg", "bachelor.deg",
                        "w.e.prove", "w.e.", "eng.listen",
                        "eng.speaking", "eng.reading", "eng.writing",
                        "math.calculus","math.algebra", "math.possibility",
                        "math.stat", "math.numericalmethod", "math.optimal",
                        "math.ml", "math.dl", "code.R", 
                        "code.python", "code.c", "otherskills",
                        "other.docs", "bio", "motivation",
                        "10yr.future", "i.promise", "consent",
                        "subscribe","get.from", "is.pass", "pass.note" )

aia.data <- aia.data[!(Timestamp %in% "取消報名，改報經理班")][!(get.from %in% "chuan")]

aia.data[, last.deg  := ifelse(post.deg %in% "",master.deg, post.deg )]
aia.data[, last.deg  := ifelse(last.deg %in% "",bachelor.deg, last.deg )]
aia.data[, is.employ := ifelse(firm.department %in% "無","非在職","在職")]

aia.data$last.gradschool <- sub(".*/ *(.*?) */.*", "\\1", aia.data$last.deg)  # between //
aia.data$last.gradyear <- sub("\\/.*","",aia.data$last.deg)  # before first /
aia.data$last.gradmajor <- sub('.*/\\s*', '', aia.data$last.deg) # after last /
aia.data$last.gradyear <- gsub('\\s+', '',aia.data$last.gradyear)

#---p.day.subs-------------------------------------
day.subs <- aia.data[, .(n.subscri = .N), by = .(date = substr(aia.data$Timestamp,1,10))]
day.subs$date <- as.Date(day.subs$date, "%d/%m/%Y" )
day.subs <- day.subs[order(date)]
day.subs$day <- weekdays(as.Date(day.subs$date))
day.subs$date.day <- paste0(day.subs$date,"-",substr(day.subs$day,1,3))

p.day.subs <- ggplot(day.subs, aes(date, n.subscri)) +
  geom_line() +
  scale_x_date(date_labels="%a\n%d", date_breaks="day", expand=c(0,0.5)) +
  facet_grid(~ paste(year(date),"-",month(date)), space="free_x", scales="free_x", switch="x") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA,colour="grey50"),
        panel.spacing=unit(0,"cm"))+
  geom_point(size = 8, color = ifelse(day.subs$day %in% c("Sunday", "Saturday"), "Red", "black")) +
  #scale_x_date(labels = date_format("%a.\n%b%d\n%Y"), breaks = round(seq(min(day.subs$date), max(day.subs$date), by = 1),1))+
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = 0.5,
                                   size =14,
                                   face="plain", 
                                   color = day.subs$color))+
  labs(x="日期",
       y="人數",
       title = "每日報名人數",
       subtitle= paste0("共",sum(day.subs$n.subscri)))+
  geom_text(aes(label = n.subscri ),
            vjust = -1,
            color = ifelse(day.subs$day %in% c("Sunday", "Saturday"), "Black", "Black"), size = text.time)+
  scale_y_continuous(limits = c(0, max(day.subs$n+10)), sec.axis = dup_axis())
#p.day.subs


#---p.day.subs.employ-------------------------------------
day.subs.employ <- aia.data[, .(n.subscri = .N), by = .(date = substr(aia.data$Timestamp,1,10), is.employ)]
day.subs.employ$date <- as.Date(day.subs.employ$date, "%d/%m/%Y" )
day.subs.employ <- day.subs.employ[order(date)]
day.subs.employ$day <- weekdays(as.Date(day.subs.employ$date))
day.subs.employ$date.day <- paste0(day.subs.employ$date,"-",substr(day.subs.employ$day,1,3))
p.day.subs.employ <- ggplot(day.subs.employ, aes(date, n.subscri, group = is.employ, color=is.employ)) +
  geom_line() +
  scale_x_date(date_labels="%a\n%d", date_breaks="day", expand=c(0,0.5)) +
  facet_grid(~ paste(year(date),"-",month(date)), space="free_x", scales="free_x", switch="x") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA,colour="grey50"),
        panel.spacing=unit(0,"cm"),
        legend.text=element_text(size=30))+
  geom_point(size = 8) +
  theme(axis.text.x = element_text(angle = 0 ,
                                   hjust = 0.5,
                                   size = 14,
                                   color ="black"))+
  labs(x="日期",
       y="人數",
       title = "每日報名人數 - 是否在職",
       subtitle= paste0("共",sum(day.subs.employ$n.subscri)))+
  geom_text(aes(label = n.subscri ), vjust = ifelse(day.subs.employ$is.employ%in% "在職", 0.4, 0.4), color = ifelse(day.subs.employ$is.employ%in% "在職", "white", "white"), size = text.time-5)+
  scale_y_continuous(limits = c(0, max(day.subs.employ$n.subscri+10)), sec.axis = dup_axis())+
  guides(fill=guide_legend(title="是否在職"))+
  scale_color_manual(values = c("#1823b2", "#61c2d3"))+
  guides(color=guide_legend(title=""))

p.age <- ggplot(age.rg.table, aes(x= age.rg, y = rg.n)) +
  geom_bar(stat = "identity", fill = "#968282") +
  labs(x="年紀",
       y="人數",
       title = "學員年紀分布",
       subtitle=paste0("共",sum(age.rg.table$rg.n),"人"))+
  geom_text(aes(label = paste0(rg.n,"位 (",round(rg.n / sum(rg.n)*100,2), "%)")), vjust = -0.5, size = bar.big)+
  scale_y_continuous(limits = c(0, max(age.rg.table$rg.n)+max(age.rg.table$rg.n)*0.1))
#---p.pyramid------------------------------

colnames(sex.temp) <- c("sex", "age", "age.rg")
sex.table <- sex.temp[, .(rg.n = .N), by = .(age.rg, sex)]
sex.table[sex %in% "男", n.per := rg.n / sum(rg.n)]
sex.table[sex %in% "女", n.per := rg.n / sum(rg.n)]

sex.vs <- c(round(sum(sex.table[sex %in% "男"]$rg.n)/sum(sex.table$rg.n)*100,2), round(sum(sex.table[sex %in% "女"]$rg.n)/sum(sex.table$rg.n)*100,2))
sex.col <- c("#1796ea","#ea1672")
sex.table$sex <- factor(sex.table$sex, levels = c("男","女"))
p.pyramid <- ggplot(data = sex.table,
                    mapping = aes(x = age.rg, fill = sex,
                                  y = ifelse(test = sex == "男", yes = -rg.n, no = rg.n))) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x="年紀",
       y="人數",
       title = "學員性別年紀分布",
       subtitle=paste0("共",sum(sex.table$rg.n),"人"))+
  guides(fill=guide_legend(title=""))+
  geom_text(aes(label = paste0(rg.n,"位 (",round(n.per*100,1), "%)"),
                hjust = ifelse( sex == "男", 1, -0)),
            size = bar.big)+
  scale_fill_manual(values=sex.col, labels = c(paste0("男",sex.vs[1],"%"), paste0("女",sex.vs[2],"%"))) +
  scale_y_continuous(labels = abs,limits = c(-max(sex.table$rg.n+10),max(sex.table$rg.n+10)))+
  theme(legend.text=element_text(size=30))

#---p.army------------------------------
army.table <- as.data.table(table(aia.data[gender %in% "男",]$army))
army.table[V1 %in% c("研發替代役服役中","服役中，研發替代役", "研發替代役, 服役中 但公司指派前來上課"), V1 := "研發替代役"]
army.table[V1 %in% "", V1 := "無提供"]
army.table[V1 %in% "未服役 2018/6後", V1 := "未服役"]
army.table <- army.table[, .(N = sum(N)), by = V1][order(N)]
#army.ref <- c(army.table[!(V1 %in% "無提供")]$V1, "無提供")
army.table <- army.table[!(V1 %in% "無提供"),]
army.table$V1 <- factor(army.table$V1, levels = army.table$V1)
army.table$col <- rep(1:2, length(nrow(army.table)))
p.army <- ggplot(army.table, aes(x=V1, y=N, fill = as.factor(col)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x="",
       y="人數",
       title = "兵役狀況",
       subtitle= paste0("共",sum(army.table$N),"/",sum(sex.table$rg.n),"人")) +
  geom_text(aes(label = paste0(N,"位 (",round(N / sum(N)*100,2), "%)"," --- ",army.table$V1)), hjust = 0, size = bar.big)+
  theme(axis.text.y =element_blank()) +
  theme(axis.ticks.y =element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0, max(army.table$N)+sd(army.table$N)*1.5))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#4B5320", "#C1C6A5"))

#---p.county------------------------------

p.county <- ggplot(county.temp, aes(x=V1, y=count, fill = as.factor(col)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x="",
       y="人數",
       title = "縣市分布",
       subtitle=paste0("共",sum(county.temp$count),"人")) +
  geom_text(aes(label = paste0(count,"位 (",round(count / sum(count)*100,2), "%)")), hjust = 0, size = bar.big)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(county.temp$count)+sd(county.temp$count)*1.5))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#359368", "#9ddcbf"))
#---p.gradyr-------------------------------------
tbl.gradyr <- as.data.table(table(aia.data$last.gradyear))
tbl.gradyr$V1 <- as.numeric(tbl.gradyr$V1)
tbl.gradyr[, yr :=paste0(substr(V1,1,3),0)]
tbl.gradyr$yr <- as.numeric(tbl.gradyr$yr)
tbl.gradyr[, yr.rg := paste0(yr, "-", yr+9)]
tbl.gradyr.tidy<- tbl.gradyr[, .(count = sum(N)), by = yr.rg]
p.gradyr <- ggplot(tbl.gradyr.tidy, aes(x= yr.rg, y = count)) +
  geom_bar(stat = "identity", fill = "#968282") +
  labs(x="畢業年份",
       y="人數",
       title = "學員最後學歷畢業年份",
       subtitle=paste0("共",sum(tbl.gradyr.tidy$count),"人"))+
  geom_text(aes(label = paste0(count,"位 (",round(count / sum(count)*100,2), "%)")), vjust = -0.5, size = bar.big)+
  scale_y_continuous(limits = c(0, max(tbl.gradyr.tidy$count)+max(tbl.gradyr.tidy$count)*0.1))
#---p.edu------------------------------
edu.table <- as.data.table(table(aia.data$highest.edu))[order(N)]
edu.table[V1 %in% "大四肄業(符合報考大學碩士班一年級新生入學考試」資格)", V1 := "大學肄業"]
edu.table[V1 %in% c("碩士在學生", "清華大學 EMBA 碩士班", "碩士學分班"), V1 := "碩士在學中"]
edu.table[V1 %in% c("延平高中", "專科."), V1 := "高中職"]
edu.table[V1 %in% c("碩士在學中", "中正大學國經所碩士就讀中"), V1 := "碩士在學"]
edu.table <- edu.table[, .(count = sum(N)), by = V1]
edu.table <- edu.table[order(count)]
edu.table$V1 <- factor(edu.table$V1, levels = edu.table$V1)
edu.table$col <- rep(1:2, nrow(edu.table))
p.edu <- ggplot(edu.table, aes( x= V1, y = count, fill = as.factor(col)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x="",
       y="人數",
       title = "最高學歷",
       subtitle=paste0("共",sum(edu.table$count),"人")) +
  geom_text(aes(label = paste0(count,"位 (",round(count / sum(count)*100,2), "%)")), hjust = 0, size = bar.big)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(edu.table$count)+sd(edu.table$count)*1.5))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#a64c79", "#9ac0ea"))


#---p.last.school------------------------------
indu.ref <- gsheet2text("https://......" )
indu.ref <- fread(indu.ref, stringsAsFactors=FALSE, encoding = "UTF-8")
tbl.last.gradschool <- as.data.table(table(aia.data$last.gradschool))[order(N)]
gradscol.ref <- tbl.last.gradschool
tbl.last.gradschool$V1 <- tolower(tbl.last.gradschool$V1)
tbl.last.gradschool$V1 <- gsub(" ", "", tbl.last.gradschool$V1)
tbl.last.gradschool$cap <- gradscol.ref$V1
map.last.school<- merge(tbl.last.gradschool, indu.ref, by.x = "V1", by.y = "firm.school", all.x = T)
map.last.school[given.name %in% c(NA,""), given.name:= cap ]
tbl.map.last.school<- map.last.school[, .(count.by.univ=sum(N)), by = .(given.name)][order(-count.by.univ)]
lastscol.bar <- tbl.map.last.school
colnames(lastscol.bar) <- c("given.name","n")
lastscol.bar <- lastscol.bar[order(n)]
lastscol.bar$given.name <- factor(lastscol.bar$given.name, levels = lastscol.bar$given.name)
lastscol.bar$col <- rep(1:2, nrow(lastscol.bar))
p.last.schol <- ggplot(lastscol.bar[n>=5], aes(x = given.name, y = n, fill = as.factor(col)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x="",
       y="人數",
       title = "最後學歷分布 (僅取>=5)",
       subtitle= paste0(sum(lastscol.bar[n>=5]$n),"/",sum(lastscol.bar$n)))+
  geom_text(aes(label = n), hjust = -0.8, size = bar.big)+
  scale_y_continuous(expand = c(0, 0),limits = c(0, max(lastscol.bar$n*1.1)), breaks = pretty_breaks())+
  scale_fill_manual(values = c("#5367e2", "#70b9ea"))+
  theme(legend.position="none")
#---p.tbl.gradschool[[i]]------------------------------
indu.ref <- gsheet2text("https://....." )
indu.ref <- fread(indu.ref, stringsAsFactors=FALSE, encoding = "UTF-8")
tbl.last.gradschool <- as.data.table(table(aia.data$last.gradschool))[order(N)]
gradscol.ref <- tbl.last.gradschool
tbl.last.gradschool$V1 <- tolower(tbl.last.gradschool$V1)
tbl.last.gradschool$V1 <- gsub(" ", "", tbl.last.gradschool$V1)
tbl.last.gradschool$cap <- gradscol.ref$V1
map.last.school<- merge(tbl.last.gradschool, indu.ref, by.x = "V1", by.y = "firm.school", all.x = T)
map.last.school[given.name %in% c(NA,""), given.name:= cap ]
tbl.map.last.school<- map.last.school[, .(count.by.univ=sum(N)), by = .(given.name)][order(-count.by.univ)]

tbl.map.last.school$given.name <- factor(tbl.map.last.school$given.name, levels = tbl.map.last.school$given.name)
p.tbl.gradschool <- list()
tmp <- nrow(tbl.map.last.school)
v1 <- seq( from =1, to = tmp, by = 30)
v2 <- v1+29
v2[length(v1)] <- tmp
colnames(tbl.map.last.school) <- c("畢業學校", "人數")
for( i in 1:length(v1) ){
  p.tbl.gradschool[[i]] <- ggtexttable(tbl.map.last.school[v1[i]: v2[i]], rows = NULL,
                                       theme = ttheme(base_style = "default", base_size = 11, base_colour = "black",
                                                      padding = unit(c(4, 4), "mm"),
                                                      colnames.style = colnames_style(size = 25 ),
                                                      rownames.style = rownames_style(size = 25),
                                                      tbody.style = tbody_style(size = 25)))
}

#---p.mjgp------------------------------
maj.group <-as.data.table(table(aia.data$last.gradmajor))
mj.gp <- gsheet2text('https://.....')
mj.gp <- fread(mj.gp, stringsAsFactors=FALSE, encoding = "UTF-8")
maj.group$V1 <- tolower(maj.group$V1)
maj.group.map <- merge(maj.group, mj.gp, by.x = "V1", by.y = "last.gradmajor", all.x = T)

tbl.maj <- maj.group.map[, .(sum.n = sum(N)), by = maj.cate][!(maj.cate %in% c(NA, ""))][order(sum.n)]
tbl.maj$maj.cate <- factor(tbl.maj$maj.cate, levels = tbl.maj$maj.cate)
tbl.maj$col <- rep(1:2, length(tbl.maj))

p.mjgp <- ggplot(tbl.maj, aes( x = maj.cate, y = sum.n, fill = as.factor(col)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x="",
       y="人數",
       title = "學員學群分布",
       subtitle= paste0(sum(tbl.maj$sum.n),"/",sum(maj.group.map$N)))+
  geom_text(aes(label = sum.n), hjust = -0.8, size = bar.big)+
  scale_y_continuous(expand = c(0, 0),limits = c(0, max(tbl.maj$sum.n*1.1)))+
  scale_fill_manual(values = c("#ea9412", "#e8c592"))+
  theme(legend.position="none")
#---p.tbl.mj[[i]]------------------------------
last.major <- aia.data[,.(last.gradschool,last.gradmajor)]
last.major$cap <- last.major$last.gradschool
last.major$last.gradschool <- gsub(" ","", last.major$last.gradschool)
last.major$last.gradschool <- tolower(last.major$last.gradschool)
gradscool.map <- merge(last.major, indu.ref, by.x = "last.gradschool", by.y = "firm.school", all.x = T)
gradscool.map[given.name %in% c(NA, ""), given.name := cap ]
grad.school.ntbl <- gradscool.map[, gradscool.n := .N, by = given.name][order(-gradscool.n)]
maj.ref <- gsheet2text('https://docs......')
maj.ref <- fread(maj.ref, stringsAsFactors=FALSE, encoding = "UTF-8")
grad.school.ntbl$maj.cap <- grad.school.ntbl$last.gradmajor
grad.school.ntbl$last.gradmajor <- tolower(grad.school.ntbl$last.gradmajor)
mj.map <- merge(grad.school.ntbl, maj.ref, by.x = "last.gradmajor", by.y = "last.gradmajor", all.x = T)
mj.maped <- mj.map[given.major %in% c("",NA), given.major := maj.cap]
tbl.mj <- mj.maped[, .(scol.maj.n = .N), by = .(given.name, given.major)]
tbl.mj[, scol.sum := sum(scol.maj.n), by = given.name]
tbl.mj <- tbl.mj[, per :=(scol.maj.n/scol.sum)][order(-scol.sum, given.name,-per)]
tbl.mj[, per2:= round(per,2)]
tbl.mj[, perr:= paste0(per2*100,"%")]
tbl.mj <- tbl.mj[order(-scol.sum, given.name, -per)]
tbl.mj.4tbl <- tbl.mj[,.(given.name, given.major,scol.maj.n,perr)]
colnames(tbl.mj.4tbl) <- c("畢業學校","系所","人數","佔該校%")
p.tbl.mj <- list()
tmp <- nrow(tbl.mj.4tbl)
v1 <- seq( from =1, to = tmp, by = 30)
v2 <- v1+29
v2[length(v1)] <- tmp

for( i in 1:length(v1) ){
  p.tbl.mj[[i]] <- ggtexttable(tbl.mj.4tbl[v1[i]: v2[i]], rows = NULL,
                               theme = ttheme(base_style = "default", base_size = 11, base_colour = "black",
                                              padding = unit(c(4, 4), "mm"),
                                              colnames.style = colnames_style(size = 25 ),
                                              rownames.style = rownames_style(size = 25),
                                              tbody.style = tbody_style(size = 25)))
}



#---p.eng[[i]]------------------------------
list.eng <- colnames(aia.data[ , grepl("eng.", names(aia.data)), with = F])
name.eng <- c("英文能力 - Listening", "英文能力 - Speaking","英文能力 - Reading", "英文能力 - Writing")
p.eng <- list ()
tbl.eng <- list()
for ( i in 1:length(list.eng)){
  tbl.eng[[i]] <- as.data.table(table(aia.data[,list.eng[i], with = F])) [order(N)]
  tbl.eng[[i]]$V1 <- factor(tbl.eng[[i]]$V1, levels = c("非常不擅長", "不擅長","普通", "擅長","非常擅長"))
  
  p.eng[[i]] <- ggplot(tbl.eng[[i]], aes( x= V1, y = N))+
    geom_bar(stat = "identity", fill = "tomato3")+
    labs(x="",
         y="人數",
         title = name.eng[i],
         subtitle=paste0("共",sum(tbl.eng[[i]]$N),"人"))+
    geom_text(aes(label=paste0(round((N/sum(N))*100,2),"%")),hjust = 0.5, vjust = -0.5, size = bar.small, color = "black")+
    geom_text(aes(label= paste(N, "位")),hjust = 0.5, vjust =-2, size = bar.big, color = "black")+
    scale_y_continuous(limits = c(0, max(tbl.eng[[i]]$N+sd(tbl.eng[[i]]$N)*1.2)))
}

#---p.code[[i]]------------------------------
list.code <- colnames(aia.data)[grepl("code.", names(aia.data))]
name.code <- c("程式設計能力 - R", "程式設計能力 - Python", "程式設計能力 - C")
p.code <- list ()
tbl.code <- list()
for ( i in 1:length(list.code)){
  tbl.code[[i]] <- as.data.table(table(aia.data[,list.code[i], with = F])) [order(N)]
  tbl.code[[i]]$V1 <- factor(tbl.code[[i]]$V1, levels = c("非常不擅長", "不擅長","普通", "擅長","非常擅長"))
  p.code[[i]] <- ggplot(tbl.code[[i]], aes( x= V1, y = N))+
    geom_bar(stat = "identity", fill = "darkgreen")+
    labs(x="",
         y="人數",
         title = name.code[i],
         subtitle=paste0("共",sum(tbl.code[[i]]$N),"人"))+
    geom_text(aes(label=paste0(round((N/sum(N))*100,2),"%")),hjust = 0.5, vjust = -0.5, size = bar.small, color = "black")+
    geom_text(aes(label= paste(N, "位")),hjust = 0.5, vjust =-2, size = bar.big, color = "black")+
    scale_y_continuous(limits = c(0, max(tbl.code[[i]]$N+sd(tbl.code[[i]]$N)*1.2)))
}
#---p.math[[i]]------------------------------
list.math <- colnames(aia.data[ , grepl("math.", names(aia.data)), with = F])
name.math <- c("數學能力 - Calculus", "數學能力 - Linear Algebra", "數學能力 - Probability", "數學能力 - Statistics",
               "數學能力 - Numerical Analysis", "數學能力 - Optimization Problem", "數學能力 - Machine Learning", "數學能力 - Deep Learning")
p.math <- list ()
tbl.math <- list()
for ( i in 1:length(list.math)){
  tbl.math[[i]] <- as.data.table(table(aia.data[,list.math[i], with = F])) [order(N)]
  tbl.math[[i]]$V1 <- factor(tbl.math[[i]]$V1, levels = c("非常不擅長", "不擅長","普通", "擅長","非常擅長"))
  
  p.math[[i]] <- ggplot(tbl.math[[i]], aes( x= V1, y = N))+
    geom_bar(stat = "identity", fill = "steelblue")+
    labs(x="",
         y="人數",
         title = name.math[i],
         subtitle=paste0("共",sum(tbl.math[[i]]$N),"人"))+
    geom_text(aes(label=paste0(round((N/sum(N))*100,2),"%")),hjust = 0.5, vjust = -0.5, size = bar.small, color = "black")+
    geom_text(aes(label= paste(N, "位")),hjust = 0.5, vjust =-2, size = bar.big, color = "black")+
    scale_y_continuous(limits = c(0, max(tbl.math[[i]]$N+sd(tbl.math[[i]]$N)*1.8)))
}


#---p.employ------------------------------
dat <- aia.data[, .(count = .N), by = is.employ]
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))
p.employ <- ggplot(dat, aes(fill=is.employ, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="學員在職與非在職佔比")+
  annotate("text",x = 3,
           y = (dat$ymax + dat$ymin)/2,
           label =dat$is.employ,
           size = 30,
           color = "black",
           fontface = "bold")+
  annotate("text",x = 3,
           y = (dat$ymax + dat$ymin)/2,
           label =paste0(round(dat$count/ sum(dat$count)*100,2),"%"),
           size = 30,
           vjust = 2,
           color = "black",
           fontface = "bold")+
  theme_bw()+
  theme(axis.text=element_blank())+
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(legend.position="none")+
  theme(axis.title=element_blank())+
  theme(plot.title = element_text(size=35))
#---p.indu------------------------------
firm.table <- as.data.table(table(aia.data$firm.department))
firm.table$V1 <- tolower(firm.table$V1)
firm.table$V1 <- gsub(" ","",firm.table$V1)
indu.map <- merge(firm.table, indu.ref[,.(firm.school, industry)], by.x = "V1", by.y = "firm.school", all.x=T)
indu.map[V1 %in% "無", industry := "無提供"  ]
indu.map[industry %in% c("", NA), industry := "其他"  ]
#indu.map[industry %in% "製造業\n", industry := "製造業"  ]
indu.tidy <- indu.map[, .(n = sum(N)), by = industry][order(-n)]
indu.tidy$industry <- factor(indu.tidy$industry, levels = c(indu.tidy[!(industry%in% c("無提供", "其他"))]$industry, "其他", "無提供" ))
indu.n.digit <- indu.tidy[ industry %in% "其他"]$n + indu.tidy[ industry %in% "無提供"]$n
p.indu <- ggplot(indu.tidy[!(industry %in% c("其他", "無提供"))], aes(x= industry, y = n, fill = industry)) +
  geom_bar(stat = "identity") +
  labs(x="",
       y="人數",
       title = "學員產業分布",
       subtitle=paste0("共",sum(indu.tidy$n)-indu.n.digit,"/",sum(indu.tidy$n),"人"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position="none") +
  geom_text(aes(label=paste0(round((n/sum(n))*100,2),"%")),hjust = 0.5, vjust = -0.5, size = bar.small, color = "black")+
  geom_text(aes(label= paste(n, "位")),hjust = 0.5, vjust =-2, size = bar.big, color = "black")+
  scale_y_continuous(limits = c(0, max(indu.tidy[!(industry %in% c("無提供","其他"))]$n+sd(indu.tidy[!(industry %in%c("無提供","其他"))]$n*1.2))))

#---p.tbl.indu[[i]]------------------------------
indu.temp <- aia.data[, .(firm.department)]
indu.temp$firm.department <- tolower(indu.temp$firm.department)
indu.temp$firm.department <- gsub(" ","",indu.temp$firm.department)
indu.temp.map <-merge(indu.temp, indu.ref, by.x= "firm.department", by.y = "firm.school", all.x = T  )
indu.temp.map[given.name %in% c("",NA) , given.name:= firm.department ]
indu.temp.map[given.name %in% "無(離職前在富邦momo電商,做預測建模、機器學習、統計學)", given.name:= "無"]
indu.p.df.temp <- indu.temp.map[, .(count = .N), by = .(given.name,industry)]
indu.p.df.temp[, indu.sum := sum(count), by = industry]
indu.p.df.temp <- indu.p.df.temp[order(-indu.sum,industry,-count)][!(industry %in% c(NA,""))]
indu.p.df.temp$indu.sum <- NULL
p.tbl.indu <- list()
tmp <- nrow(indu.p.df.temp)
v1 <- seq( from =1, to = tmp, by = 30)
v2 <- v1+29
v2[length(v1)] <- tmp
colnames(indu.p.df.temp) <- c("單位名稱","產業別","人數")
for( i in 1:length(v1) ){
  p.tbl.indu[[i]] <- ggtexttable(indu.p.df.temp[v1[i]: v2[i]], rows = NULL,
                                 theme = ttheme(base_style = "default", base_size = 11, base_colour = "black",
                                                padding = unit(c(4, 4), "mm"),
                                                colnames.style = colnames_style(size = 25 ),
                                                rownames.style = rownames_style(size = 25),
                                                tbody.style = tbody_style(size = 25)))
}

#---p.occu------------------------------
occu.ref <- gsheet2text("https://...." )
occu.ref <- fread(occu.ref, stringsAsFactors=FALSE, encoding = "UTF-8")
occu.table <- as.data.table(table(aia.data$occupation))
occu.table$V1 <- tolower(occu.table$V1)
occu.table$V1 <- gsub(" ", "", occu.table$V1)
occu.map <- merge(occu.table, occu.ref[,.(occu, category)], by.x = "V1", by.y = "occu", all.x = T)
occu.map[V1 %in% "無", category := "無提供"  ]
occu.map[category %in% c("", NA), category := "其他"  ]
occu.tidy <- occu.map[, .(n = sum(N)), by = category][order(-n)]
occu.tidy$category <- factor(occu.tidy$category, levels = c(occu.tidy[!(category %in% c("無提供", "其他"))]$category, "其他", "無提供" ))
p.occu <- ggplot(occu.tidy[!(category %in%c("其他", "無提供"))], aes(x= category, y = n, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x="",
       y="人數",
       title = "學員職業分布",
       subtitle=paste0("共",sum(occu.tidy[!(category %in% c("其他","無提供"))]$n),"/",sum(occu.tidy$n),"人"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position="none") +
  geom_text(aes(label=paste0(round((n/sum(n))*100,2),"%")),hjust = 0.5, vjust = -0.5, size = bar.small, color = "black")+
  geom_text(aes(label= paste(n, "位")),hjust = 0.5, vjust =-2, size = bar.big, color = "black")+
  scale_y_continuous(limits = c(0, max(occu.tidy[!(category%in%c("無提供","其他")),]$n)+sd(occu.tidy[!(category%in%c("無提供","其他"))]$n)*1.2))




###---createPDF-------------------
img <- readPNG("AIA/aia.report.png")
g <- rasterGrob(img, interpolate=TRUE)
df <- data.frame()
Sys.time()
cover <-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme_bw()+
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(legend.position="none")+
  theme(axis.title=element_blank())+
  theme(axis.text=element_blank())+
  annotate("text",x = 0,
           y = 80,
           label =paste(Sys.time(),"共", nrow(aia.data),"人"),
           size = 8,
           color = "black",
           fontface = "bold",
           hjust = 0)+
  annotate("text",x = 0,
           y = 85,
           label ="Applicant Background Information",
           size = 18,
           color = "black",
           fontface = "bold",
           hjust = 0)+
  annotate("text",x = 0,
           y = 95,
           label =paste0(class.ch.name),
           size = 30,
           color = "black",
           fontface = "bold",
           hjust = 0)
cairo_pdf(paste0("/home/katieyth/AIA/",class.name,".pdf"),  onefile = TRUE ,width = 25, height = 15)
plot(cover)
layout <- matrix(c( 1,2),2,1,byrow=TRUE)
multiplot(p.day.subs,p.day.subs.employ, layout=layout)

layout <- matrix(c( 1,2),2,1,byrow=TRUE)
multiplot(p.age,p.pyramid, layout=layout)

plot(p.army)
plot(p.county)

layout <- matrix(c( 1,2),2,1,byrow=TRUE)
multiplot(p.gradyr,p.edu, layout=layout)

plot(p.last.schol)

# ifelse(odd(length(p.tbl.gradschool)),
#        for (i in 1:((length(p.tbl.gradschool)-1)/2)){
#          layout <- matrix(c(1,2),1,2,byrow=TRUE)
#          multiplot(p.tbl.gradschool[[i*2-1]], p.tbl.gradschool[[i*2]],layout=layout)
#        } ,
#        for (i in 1:(length(p.tbl.gradschool)/2)){
#          layout <- matrix(c(1,2),1,2,byrow=TRUE)
#          multiplot(p.tbl.gradschool[[i*2-1]], p.tbl.gradschool[[i*2]],layout=layout)}
# )
# 
# if(odd(length(p.tbl.gradschool)))plot(p.tbl.gradschool[[length(p.tbl.gradschool)]])


for (i in 1:length(p.tbl.gradschool)){
  plot(p.tbl.gradschool[[i]])
}


plot(p.mjgp)

for (i in 1:length(p.tbl.mj)){
  plot( p.tbl.mj[[i]])
}

layout <- matrix(c( 1,2,3,4),2,2,byrow=TRUE)
multiplot(p.eng[[1]], p.eng[[2]], p.eng[[3]], p.eng[[4]], layout=layout)


layout <- matrix(c( 1,2,3),3,1,byrow=TRUE)
multiplot(p.code[[1]], p.code[[2]], p.code[[3]], layout=layout)

layout <- matrix(c( 1,2,3,4,5,6,7,8),4,2,byrow=TRUE)
multiplot(p.math[[1]], p.math[[2]], p.math[[3]], p.math[[4]],
          p.math[[5]], p.math[[6]], p.math[[7]], p.math[[8]], layout=layout)

plot(p.employ)
plot(p.indu)


# ifelse((length(p.tbl.indu)%%2 !=0),
#        for (i in 1:((length(p.tbl.indu)-1)/2)){
#          layout <- matrix(c( 1,2),1,2,byrow=TRUE)
#          multiplot(p.tbl.indu[[i*2-1]], p.tbl.indu[[i*2]],layout=layout)
#        } ,
#        for (i in 1:(length(p.tbl.indu)/2)){
#          layout <- matrix(c( 1,2),1,2,byrow=TRUE)
#          multiplot(p.tbl.indu[[i*2-1]], p.tbl.indu[[i*2]],layout=layout)}
# )
# 
# if(length(p.tbl.indu)%%2 !=0)plot(p.tbl.indu[[length(p.tbl.indu)]])


for (i in 1:length(p.tbl.indu)){
  plot(p.tbl.indu[[i]])
}

plot(p.occu)

dev.off()

