##########  Mobility: Data Analysis   ###########
##########  Andrew Piper    #####################

#read in derived data

a<-read.csv(gzfile("all.csv.gz"))
c<-a[a$collection == "conlit",]

#### Final table for paper #####

#measures
# measures<-c("avg_Distance_GPE_Tokens", "avg_Distance_GPE", "num_gpe_places_norm_byCharacter",
#             "num_nongpe_places_norm_byCharacter", "num_gpe_places_norm", "num_nongpe_places_norm",
#             "deixis_count_perplace", "semantic_dist_mean")
# 
# #more intuitive names used for measures
# measure.names<-c("Distance_per_Token", "Distance_per_GPE", "GPE_per_Character",
#                  "nonGPE_per_Character", "GPE_per_Token", "nonGPE_per_Token",
#                  "Generics_per_nonGPE","avg_semantic_distance", "nonGPE_GPE_Ratio")

#variables
measures<-c("dist_miles", "gpe_places_total", "nongpe_places_total",
            "deixis_count_perplace", "semantic_dist_mean")

#more intuitive names used for measures
measure.names<-c("Distance", "GPEs", "NonGPEs",
                 "Deictics","Semantic Dist", "NON_GPE Ratio")

#social classes we are analyzings
classes<-c("Fictionality", "Prestige", "Youth", "Female Character")

#regression functions
perform_regression_opposite <- function(data, formula, formula2, category, measure_name) {
  model <- summary(lm(formula, data = data))
  R2 <- round(model$r.squared, 3)
  co.df <- model$coefficients
  p <- round(co.df[2,4], 5)
  p.code <- if (p < .001) {"***"} else if (p < 0.01) {"**"} else if (p < 0.05) {"*"} else {"."}
  model2<-t.test(formula2, data=data)
  Mean.Group1<-round(model2$estimate[[1]],3)
  Mean.Group2<-round(model2$estimate[[2]], 3)
  #Estimate_Actual<-co.df[1,1]
  #Difference_Actual<--co.df[2,1]
  #Estimate <- round(co.df[1,1], 3)
  Difference <- -co.df[2,1]
  Valence <- if (Difference < 0) {"-"} else {"+"}
  return(data.frame(Category = category, Measure = measure_name, Mean.Group1, Mean.Group2, Valence, R2, p, p.code))
}

perform_regression_same <- function(data, formula, formula2, category, measure_name) {
  model <- summary(lm(formula, data = data))
  R2 <- round(model$r.squared, 3)
  co.df <- model$coefficients
  p <- round(co.df[2,4], 5)
  p.code <- if (p < .001) {"***"} else if (p < 0.01) {"**"} else if (p < 0.05) {"*"} else {"."}
  #Estimate <- round(co.df[1,1], 3) + round(co.df[2,1], 3)
  Difference <- co.df[2,1]
  model2<-t.test(formula2, data=data)
  Mean.Group1<-round(model2$estimate[[2]], 3)
  Mean.Group2<-round(model2$estimate[[1]], 3)
  #Difference<-Estimate.Group1-Estimate.Group2
  Valence <- if (Difference < 0) {"-"} else {"+"}
  return(data.frame(Category = category, Measure = measure_name, Mean.Group1, Mean.Group2, Valence, R2, p, p.code))
}

#no scientific notation
options(scipen = 999)

#run
final.df<-NULL

for (i in 1:length(classes)){
  
  #Fictionality
  if (classes[i] == classes[1]){
    #prepare data
    class_data<-c
    for (j in 1:length(measures)){
      #run regression
      temp.df<-perform_regression_opposite(class_data, as.formula(paste0(measures[j], " ~ Category+Genre+perspective+Tokens+char_count")), as.formula(paste0(measures[j], " ~ Category")), classes[i], measure.names[j])
      final.df<-rbind(final.df, temp.df)
    }
    #prepare data
    class_data<-c[!is.infinite(c$non_gpe_ratio),]
    temp.df<-perform_regression_opposite(class_data, as.formula(paste0("non_gpe_ratio", " ~ Category+Genre+perspective+Tokens+char_count")), as.formula(paste0("non_gpe_ratio", " ~ Category")),classes[i], "nonGPE_GPE_Ratio")
    final.df<-rbind(final.df, temp.df)
  }
  
  #Prestige
  if (classes[i] == classes[2]){
    #prepare data
    class_data<-c[c$Genre %in% c("PW", "BS"),]
    for (j in 1:length(measures)){
      #run regression
      temp.df<-perform_regression_same(class_data, as.formula(paste0(measures[j], " ~ Genre+perspective+Tokens+char_count")), as.formula(paste0(measures[j], " ~ Genre")),classes[i], measure.names[j])
      final.df<-rbind(final.df, temp.df)
    }
    #prepare data
    class_data<-class_data[!is.infinite(class_data$non_gpe_ratio),]
    temp.df<-perform_regression_same(class_data, as.formula(paste0("non_gpe_ratio", " ~ Genre+perspective+Tokens+char_count")), as.formula(paste0("non_gpe_ratio", " ~ Genre")), classes[i], "nonGPE_GPE_Ratio")
    final.df<-rbind(final.df, temp.df)
  }
  
  #Youth
  if (classes[i] == classes[3]){
    #prepare data
    class_data<-c[c$Genre %in% c("NYT", "BS", "PW", "MID"),]
    class_data$Adult<- ifelse(class_data$Genre == "MID", "MID", "AD")
    for (j in 1:length(measures)){
      #run regression
      temp.df<-perform_regression_same(class_data, as.formula(paste0(measures[j], " ~ Adult+perspective+Tokens+char_count")), as.formula(paste0(measures[j], " ~ Adult")), classes[i], measure.names[j])
      final.df<-rbind(final.df, temp.df)
    }
    #prepare data
    class_data<-class_data[!is.infinite(class_data$non_gpe_ratio),]
    temp.df<-perform_regression_same(class_data, as.formula(paste0("non_gpe_ratio", " ~ Adult+perspective+Tokens+char_count")), as.formula(paste0("non_gpe_ratio", " ~ Adult")), classes[i], "nonGPE_GPE_Ratio")
    final.df<-rbind(final.df, temp.df)
  }
  
  #Female Character
  if (classes[i] == classes[4]){
    #prepare data
    class_data<-c[c$Category == "FIC",]
    class_data<-class_data[class_data$inf_gender %in% c("she/her/hers", "he/him/his"),]
    for (j in 1:length(measures)){
      #run regression
      temp.df<-perform_regression_same(class_data, as.formula(paste0(measures[j], " ~ inf_gender+Genre+perspective+Tokens+char_count")), as.formula(paste0(measures[j], " ~ inf_gender")), classes[i], measure.names[j])
      final.df<-rbind(final.df, temp.df)
    }
    #prepare data
    class_data<-class_data[!is.infinite(class_data$non_gpe_ratio),]
    temp.df<-perform_regression_same(class_data, as.formula(paste0("non_gpe_ratio", " ~ inf_gender+Genre+perspective+Tokens+char_count")), as.formula(paste0("non_gpe_ratio", " ~ inf_gender")), classes[i], "nonGPE_GPE_Ratio")
    final.df<-rbind(final.df, temp.df)
  }
}

###### Start Finish Comparison ######

#Number 0 values (start = finish)
x<-nrow(c[c$Start_Finish_Miles == 0 & c$Category == "FIC",])
y<-nrow(c[c$Category == "FIC",])
m<-nrow(c[c$Start_Finish_Miles == 0 & c$Category == "NON",])
n<-nrow(c[c$Category == "NON",])
chisq.test(data.frame(c(x,y), c(m,n)))

#compare start finish difference
summary(lm(Start_Finish_Miles ~ Category+Genre+perspective+Tokens+char_count, data=c))
summary(lm(Start_Finish_Z ~ Category+Genre+perspective+Tokens+char_count, data=c))
summary(lm(first_last_SemanticDist ~ Category+Genre+perspective+Tokens+char_count, data=c))

#############  Historical Data   #############
b<-a[a$collection != "conlit",]

#make yearly table
time.df<-NULL
for (i in 1:nlevels(factor(b$pub_date))){
  sub<-b[b$pub_date == levels(factor(b$pub_date))[i],]
  pub_date<-sub$pub_date[1]
  distance<-mean(sub$dist_miles, ra.rm=T)
  dist_norm_Tokens<-mean(sub$dist_miles_norm, na.rm=T)
  dist_norm_Characters<-mean(sub$dist_miles_norm_byCharacter, na.rm=T)
  GPEs<-mean(sub$num_gpe_places, na.rm=T)
  GPEs_norm_Tokens<-mean(sub$num_gpe_places_norm, na.rm=T)
  GPEs_norm_Character<-mean(sub$num_gpe_places_norm_byCharacter, na.rm=T)
  temp.df<-data.frame(pub_date, distance, GPEs, dist_norm_Tokens, dist_norm_Characters, GPEs_norm_Tokens, GPEs_norm_Character)
  time.df<-rbind(time.df, temp.df)
}

library(ggplot2)
ggplot(time.df, aes(x=pub_date, y=GPEs_norm_Character)) +
  geom_point() +
  theme_classic() + 
  xlab("Year") +
  ylab("Distance (Miles)") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.8, 0.2)) +
  theme(legend.text = element_text(size=12))+
  #theme(legend.position="bottom") +
  #ggtitle("Percentage of ")+
  geom_smooth(method = "lm") +
  labs(caption="Source: ")




###### Individual Analyses ######

#Character Count per token
hist(c$char_count_norm)
t.test(char_count_norm ~ Category, data=c)
boxplot(char_count_norm ~ Category, data=c)
wilcox.test(char_count_norm ~ Category, data=c)
summary(lm(char_count_norm ~ Category + Genre, data=c))

#Distance per GPE
hist(c$avg_Distance_GPE)
boxplot(avg_Distance_GPE ~ Category, data=c)
summary(lm(avg_Distance_GPE ~ Category+Genre, data=c))

#Distance per token
hist(c$avg_Distance_GPE_Tokens)
summary(lm(avg_Distance_GPE_Tokens ~ Category+Genre, data=c))
summary(lm(dist_miles ~ Category+Genre+Tokens+char_count, data=c))

#Number GPE per character mention
summary(lm(num_gpe_places_norm_byCharacter ~ Category + Genre, data=c))

#Number NonGPE per character mention
summary(lm(num_nongpe_places_norm_byCharacter ~ Category + Genre, data=c))

#Number GPE per token
summary(lm(num_gpe_places ~ Category + Genre + Tokens + char_count, data=c))

#Number NonGPE per token
summary(lm(num_nongpe_places ~ Category + Genre + Tokens + char_count, data=c))


#First / Last semantic distance for GPEs
summary(lm(first_last_SemanticDist ~ Category+Genre, data=c))
fic<-c[c$Category == "FIC",]
summary(lm(first_last_SemanticDist ~ Genre, data=fic))
nrow(fic[fic$first_last_SemanticDist == 0,])/nrow(fic)
fic0<-fic[fic$first_last_SemanticDist == 0,]
table(fic0$Genre)
df<-data.frame(table(fic0$Genre), table(fic$Genre))
df<-df[,-3]
chisq.test(df[,2:3])

#type token ratio GPE
c.test<-c[-which(c$ttr_gpe == 0 | c$ttr_gpe == 1),]
summary(lm(ttr_gpe ~ Category+Genre, data=c.test))

#ttr for nongpe
c.test<-c[-which(c$ttr_nongpe == 0 | c$ttr_nongpe == 1),]
summary(lm(ttr_nongpe ~ Category+Genre, data=c.test))

#non/gpe ratio
c.test <- c[!is.infinite(c$non_gpe_ratio),]
summary(lm(non_gpe_ratio ~ Category+Genre, data=c.test))

#deixis
summary(lm(deixis_count_perplace ~ Category+Genre, data=c))

#semantic distance
summary(lm(semantic_dist_mean ~ Category+Genre, data=c))

#Distance by gender
c.test<-c[c$inf_gender %in% c("she/her", "he/him/his"),]
c.fic<-c.test[c.test$Category == "FIC",]
c.fic.adult<-c.fic[!c.fic$Genre %in% c("MID", "YA"),]
summary(lm(avg_Distance_GPE ~ inf_gender + Category+Genre, data=c.test))
summary(lm(avg_Distance_GPE_Tokens ~ inf_gender + Category+Genre, data=c.test))
summary(lm(avg_Distance_GPE_Tokens ~ inf_gender + Genre, data=c.fic.adult))

summary(lm(dist_miles ~ inf_gender + Category+Genre, data=c.test))
summary(lm(dist_miles ~ inf_gender + Genre, data=c.fic.adult))
t.test(avg_Distance_GPE_Tokens ~ inf_gender, data=c.fic.adult)
t.test(avg_Distance_GPE_Tokens ~ inf_gender, data=c.fic)
wilcox.test(avg_Distance_GPE_Tokens ~ inf_gender, data=c.fic.adult)
median(c.fic.adult$avg_Distance_GPE_Tokens[c.fic.adult$inf_gender == "he/him/his"])
median(c.fic.adult$avg_Distance_GPE_Tokens[c.fic.adult$inf_gender == "she/her"])
hist(c.fic.adult$avg_Distance_GPE_Tokens)
t.test(avg_Distance_GPE ~ inf_gender, data=c.fic.adult)
wilcox.test(avg_Distance_GPE ~ inf_gender, data=c.fic.adult)
t.test(num_gpe_places_norm_byCharacter ~ inf_gender, data=c.fic.adult)
wilcox.test(num_gpe_places_norm_byCharacter ~ inf_gender, data=c.fic.adult)
#remove romance
c.fic.adult2<-c.fic.adult[c.fic.adult$Genre != "ROM",]
t.test(avg_Distance_GPE_Tokens ~ inf_gender, data=c.fic.adult2)
wilcox.test(avg_Distance_GPE_Tokens ~ inf_gender, data=c.fic.adult2)

#physical distance by genre controlling for gender
c.fic.adult.she<-c.fic.adult[c.fic.adult$inf_gender == "she/her",]
model <- aov(avg_Distance_GPE_Tokens ~ Genre, data = c.fic.adult.she)
tukey_result <- TukeyHSD(model)
plot(tukey_result, las=2)
sort(tapply(c$avg_Distance_GPE_Tokens, c$Genre, median))
sort(tapply(c$avg_Distance_GPE, c$Genre, median))
plot(avg_Distance_GPE_Tokens ~ factor(Genre), data = c.fic.adult.she)
plot(avg_Distance_GPE_Tokens ~ factor(Genre), data = c.fic.adult)

#semantic distance by genre
sort(tapply(c$semantic_dist_mean, c$Genre, median))
summary(lm(semantic_dist_mean ~ Genre, data=c))

#correlation matrix of GPE and NON
keep<-c("num_nongpe_places_norm", "num_nongpe_places_norm_byCharacter", "num_gpe_places_norm",
        "num_gpe_places_norm_byCharacter")
c.test<-c[, colnames(c) %in% keep]
correlation_matrix <- cor(c.test)

# Load the 'corrplot' package for plotting
# You may need to install it using install.packages("corrplot") if you haven't already
library(corrplot)

# Create a correlation plot
corrplot(correlation_matrix, method = "color")

#make FIC table
fic<-c[c$Category == "FIC",]
meta.f<-meta[meta$Category == "FIC",]
meta.f<-meta.f[order(meta.f$ID),]
fic<-fic[order(fic$book_id),]
meta.f$ID2<-gsub(".txt", "", meta.f$ID)
which(fic$book_id != meta.f$ID2)
fic$protagonist_concentration<-meta.f$protagonist_concentration
fic$goodreads_avg<-meta.f$goodreads_avg
fic$total_ratings<-meta.f$total_ratings
fic.adult<-fic[!fic$Genre %in% c("MID", "YA"),]

#association of Goodreads ratings with spatial features
#summary(lm(goodreads_avg ~ non_minus_gpe_avg_norm+Genre, data = fic))
#summary(lm(log(total_ratings) ~ non_minus_gpe_avg_norm+Genre, data = fic))
summary(lm(log(total_ratings) ~ avg_Distance_GPE_Tokens+Genre, data = fic))
summary(lm(goodreads_avg ~ avg_Distance_GPE_Tokens+Genre, data = fic))
summary(lm(goodreads_avg ~ dist_miles+Genre+Tokens, data = fic))
summary(lm(goodreads_avg ~ avg_Distance_GPE+Genre, data = fic))

#association of Character concentration with spatial features
summary(lm(avg_Distance_GPE ~ protagonist_concentration + Genre, data = fic))
summary(lm(dist_miles_allChars_norm_Tokens ~ protagonist_concentration + Genre, data = fic))
summary(lm(num_gpe_places_allChars_norm_Tokens ~ protagonist_concentration + Genre, data = fic))










