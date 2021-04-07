###############################################
#SETUP
###############################################
library("VIM")
install.packages("miscset", dependencies = T)
library("miscset")
install.packages("tidyverse")
library("tidyverse")
install.packages("ggplot2", dependencies = TRUE)
library("ggplot2")

setwd("C:/Users/Niamh/OneDrive/Documents/FYP")
clavis_data = read.csv("data.csv", stringsAsFactors = T)
clavis_data$Date = as.Date(clavis_data$Date)
clients = unique(clavis_data$Client)
tasks_per_client =aggregate(Task ~ Client, data = clavis_data, function(x) length(unique(x)))
projects_per_client =aggregate(Project ~ Client, data = clavis_data, function(x) length(unique(x)))
employees_per_client = aggregate(Name ~ Client, data = clavis_data, function(x) length(unique(x)))
total_hours_per_client = aggregate(Hours ~ Client, data = clavis_data, sum)
average_hours_per_client = aggregate(Hours ~ Client, data = clavis_data, mean)
client_data = data.frame(clients, projects_per_client$Project, tasks_per_client$Task, employees_per_client$Name,
                         total_hours_per_client$Hours, average_hours_per_client$Hours)
colnames(client_data) = c("Client" , "No.Projects", "No.Tasks", "No.Employees", "Total Hours", "Average Hours")
client_data[, "hrs/employees"] = client_data[, "Total Hours"] / client_data[, "No.Employees"]

task_hrs = unique(clavis_data$Task)
test = clavis_data

client_tasks <- setNames(data.frame(matrix(ncol = 119, nrow = 111)), task_hrs)
row.names(client_tasks) <- unique(clavis_data$Client)
client_tasks[is.na(client_tasks)] <- 0

grouped_tasks <- setNames(data.frame(matrix(ncol = 10, nrow = 111)), c("Change Request", "Data Ops",
                 "Delivery", "eCC", "GCS", "Internal", "Project Management", "Refresh -", "Setup -", "Misc"))
row.names(grouped_tasks) <- unique(clavis_data$Client)
grouped_tasks[is.na(grouped_tasks)] <- 0
substr(clavis_data$Task, start =1, stop = 3)
isFALSE(any(substr(colnames(grouped_tasks)[4], start =1, stop = 3), 
            substr("eCC Sales Support", start =1, stop =3)))
install.packages(("stringi"))
library("stringi")

stri_detect_fixed(clavis_data$Task, colnames(grouped_tasks))

for (j in 1:ncol(grouped_tasks)) {
  for (i in 1:nrow(grouped_tasks)) {
    if (colnames(grouped_tasks)[j] != "Misc") {
      temp_sub = subset(clavis_data, clavis_data$Client == rownames(grouped_tasks)[i] 
                        & grepl(colnames(grouped_tasks)[j], clavis_data$Task), fixed = T)
      temp_hrs = sum(temp_sub$Hours)
      grouped_tasks[i,j] = temp_hrs
      }
    else {
      temp <- subset(clavis_data, clavis_data$Client == rownames(grouped_tasks)[i])
      total <- sum(temp$Hours)
     grouped_tasks[i,j] <- total - sum(grouped_tasks[i,1:9])
    }
  }
}
unique(clavis_data$Task)
grouped_tasks[10,]
x <-subset(clavis_data, clavis_data$Client == "Client_10")
sum(x$Hours)
saf <- x %>%
  group_by(Task) %>%
  summarise(sum(Hours)) 

test1 <- cbind(client_data, grouped_tasks)
  #for (i in 1:nrow(client_tasks)) {
 # for (j in 1:ncol(client_tasks)) { 
#    temp_sub =subset(clavis_data, clavis_data$Client == rownames(client_tasks)[i] & clavis_data$Task == colnames(client_tasks)[j])
#    temp_hrs = sum(temp_sub$Hours)
#    client_tasks[i,j] = temp_hrs
#  } 
#}
client_data

test = cbind(client_data, client_tasks)
client_data = test1

stats = sapply(client_data, function(x) summary(x))
ggplot(client_data, aes(x = No.Tasks, y= No.Employees)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="No. Employees vs No. Tasks")  
cor(client_data$No.Employees, client_data$No.Tasks)

plot(client_data$`hrs/employees`)
client_data %>% 
  dplyr::arrange(desc(client_data$`Total Hours`)) %>%
  head(20)

ggplot(client_data, aes(x = `Total Hours`, y= No.Employees)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Total Hours vs No. Employees") +
  xlim(0,4000)
  test = subset(client_data, `Total Hours` <4000)
cor(test$`Total Hours`, test$No.Employees)

barplot(client_data$`Total Hours`)
temp = client_data %>% dplyr::arrange(desc(client_data$`Total Hours`))
plot(temp$`Total Hours`, xlim)
temp2 = client_data %>% dplyr::arrange(desc(client_data$`Average Hours`))
plot(temp2$`Average Hours`)
#CLAVIS eCOMMERCE ENGINEERING HAS 56 EMPLOYEES, OUTLIER
#CLIENT 101 USES 47 EMPLOYEES, OUTLIERS
#CLIENT 36 USES 39 EMPLOYEES, POSSIBLE OUTLIER



###############################################
#DENDOGRAM
###############################################
client_dendo = dist(scale(test1[,2:17]), method = "euclidean")
avg = hclust(client_dendo, method = "average")
comp =hclust(client_dendo, method = "complete")
ward =hclust(client_dendo, method = "ward.D")
single =hclust(client_dendo, method = "single")

#IS THIS MOST APPROPRIATE? COULD USE AVERAGE LINKAGE? 
plot(comp, cex = 0.6, hang = -1)
#AVERAGE HEIGHT PLUS 3SD
avg_height <- mean(comp$height)
std_height <- sd(comp$height)
cut_off = avg_height + 3*std_height

#rect.hclust(hc1, k=3, border = 2:6)
abline(h=cut_off, col="red")

###############################################
#NUMBER OF CLUSTERS
###############################################
c1 <- cutree(avg, k=5)
client_data$avg = c1

c2 <- cutree(comp, k=4)
client_data$comp = c2

c3 <- cutree(ward, k=3)
client_data$ward = c3

c4 <- cutree(single, k=4)
client_data$single = c4

Rand_Ind <- setNames(data.frame(matrix(ncol = 4, nrow = 3)), c("Avg", "Comp", "Ward", "Single"))
row.names(Rand_Ind) <- c("Avg", "Comp", "Ward")
Rand_Ind[is.na(Rand_Ind)] <- 0

Rand_Ind[1,2] = adjustedRandIndex(c1,c2)
Rand_Ind[1,3] = adjustedRandIndex(c1,c3)
Rand_Ind[1,4] =adjustedRandIndex(c1,c4)
Rand_Ind[2,4] =adjustedRandIndex(c2,c4)
Rand_Ind[2,3] =adjustedRandIndex(c2,c3)
Rand_Ind[3,4] =adjustedRandIndex(c3,c4)

adjustedRandIndex(cutree(ward, k=4), client_clustering$classification)
#added column to deal with cluster 

###############################################
#MCLUST
###############################################
install.packages(mclust, dependencies = T)
library(mclust)

client_clustering = Mclust(scale(test1[,2:17]))
#G = 3/4 set number of clusters 
summary(client_clustering)
plot(client_clustering$classification)
plot(client_clustering, what = "BIC")
plot(client_clustering, what = "classification", dimens = c(4,3))
summary(client_clustering$BIC)


###############################################
#EXAMINING CLUSTERS
###############################################
install.packages("miscset", dependencies = T)
library(miscset)
install.packages("tidyverse")
library(tidyverse)
client_data$mclust = client_clustering$classification

clavis_client_merge = merge.data.frame(clavis_data, client_data, by = "Client")
(clavis_client_merge$Cluster)

#CLUSTER ONE
cl_cluster1 = subset(client_data, mclust == 1)
total_hrs_client = sapply(cl_cluster1[,9:18], sum)
sort(total_hrs_client, decreasing = T)

#CLUSTER TWO
cl_cluster2 = subset(client_data, mclust == 2)
total_hrs_client = sapply(cl_cluster2[,9:18], sum)
sort(total_hrs_client, decreasing = T)
median(cl_cluster2$No.Employees)

#CLUSTER THREE
cl_cluster3 = subset(client_data, mclust == 3)
total_hrs_client = sapply(cl_cluster3[,9:18], sum)
sort(total_hrs_client, decreasing = T)
mean(cl_cluster3$No.Employees)
sum(total_hrs_client)
nrow(cl_cluster3)

#CLUSTER FOUR
cl_cluster4 = subset(client_data, mclust == 4)
total_hrs_client = sapply(cl_cluster4[,9:18], sum)
sort(total_hrs_client, decreasing = T)
mean(cl_cluster4$No.Employees)
sum(total_hrs_client)
nrow(cl_cluster4)

#CLUSTER FIVE
cl_cluster5 = subset(client_data, mclust == 5)
total_hrs_client = sapply(cl_cluster5[,9:18], sum)
sort(total_hrs_client, decreasing = T)
median(cl_cluster5$No.Employees)
sum(total_hrs_client)
nrow(cl_cluster5)

client_data %>%
  dplyr::arrange(desc(`Total Hours`)) %>%
  head(20)


