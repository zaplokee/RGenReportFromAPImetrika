library(httr)
library(jsonlite)
library(utils)
library(stringr)


#Authentication things
appId<-"YOUR_APPLICATION_ID"
appSecret<-"PASSWORD"
appToken<-"TOKEN"


#get counterID
req<-paste("http://api-metrika.yandex.ru/counters.json?oauth_token=", appToken, sep="")
counters<-fromJSON(txt=req)
rows<-as.integer(counters$rows)
counters<-counters$counters
counters_data<-data.frame(num=1:rows, site=counters$site)
counters_data$id<-counters$id
counters_data$name<-counters$name
counters_data$status<-counters$code_status
ask_for_counter<-as.character()
for (i in 1:rows) {
    ask_for_counter<-paste(ask_for_counter, "[", i, "] - ", counters_data[i,][,"site"], "\n", sep="")
}
ask_for_counter<-paste(ask_for_counter, "Choose counter, specify the index: ", sep="")
read_num<-as.integer(readline(prompt=ask_for_counter))
counterID<-counters_data[read_num,][,"id"]


#get date
ask_for_year<-as.character("Year (4 numbers): ")
read_year<-as.character(readline(prompt=ask_for_year))
ask_for_month<-as.character("Month (2 numbers): ")
read_month<-as.character(readline(prompt=ask_for_month))

gen_day2 <- function(mon, year) {
    if(mon=="01" | mon=="03" | mon=="05" | mon=="07" | mon=="08" | mon=="10" | mon=="12") {
        day2<-31
        return(day2)
    } else if(mon=="04" | mon=="06" | mon=="09" | mon=="11") {
        day2<-30
        return(day2)
    } else if(mon=="02") {
        if(as.integer(year)%%4==0) {
            day2<-29
        } else { day2<-28 }
    
    } else {
        print("Wrong number specified, cannot calculate day2")

    
}

mons_years<-paste(read_month, "-", read_year, sep="")
date1<-paste(read_year, read_month, "01", sep="")
date2<-paste(read_year, read_month, as.character(gen_day2(read_month, read_year)), sep="")

#generate prev_date1 and prev_date2 for comparing
prev_year<-as.character(as.integer(read_year)-1)
prev_date1<-character()
prev_date2<-character()
if (as.integer(read_month)>3) {
    prev_months<-c(as.character(as.integer(read_month)-1), as.character(as.integer(read_month)-2), as.character(as.integer(read_month)-3))
    for (i in 1:length(prev_months)) {
        if(as.integer(prev_months[i])<10) {
            prev_months[i]<-paste("0", prev_months[i], sep="")
        }
        prev_date1<-c(prev_date1, paste(read_year, prev_months[i], "01", sep=""))
        prev_date2<-c(prev_date2, paste(read_year, prev_months[i], as.character(gen_day2(prev_months[i], read_year)), sep=""))
        mons_years<-c(mons_years, paste(prev_months[i], "-", read_year, sep=""))
    }
    
} else if(as.integer(read_month)==3) {
    prev_months<-c("02", "01", "12")
    prev_date1<-c(prev_date1, paste(read_year, prev_months[1], "01", sep=""), paste(read_year, prev_months[2], "01", sep=""), paste(prev_year, prev_months[3], "01", sep=""))
    prev_date2<-c(prev_date2, paste(read_year, prev_months[1], as.character(gen_day2(prev_months[1], read_year)), sep=""), paste(read_year, prev_months[2], as.character(gen_day2(prev_months[2], read_year)), sep=""), paste(prev_year, prev_months[3], as.character(gen_day2(prev_months[3], prev_year)), sep=""))
    mons_years<-c(mons_years, paste(prev_months[1], "-", read_year, sep=""), paste(prev_months[2], "-", read_year, sep=""), paste(prev_months[3], "-", prev_year, sep=""))
} else if(as.integer(read_month)==2) {
    prev_months<-c("01", "12", "11")
    prev_date1<-c(prev_date1, paste(read_year, prev_months[1], "01", sep=""), paste(prev_year, prev_months[2], "01", sep=""), paste(prev_year, prev_months[3], "01", sep=""))
    prev_date2<-c(prev_date2, paste(read_year, prev_months[1], as.character(gen_day2(prev_months[1], read_year)), sep=""), paste(prev_year, prev_months[2], as.character(gen_day2(prev_months[2], prev_year)), sep=""), paste(prev_year, prev_months[3], as.character(gen_day2(prev_months[3], prev_year)), sep=""))
    mons_years<-c(mons_years, paste(prev_months[1], "-", read_year, sep=""), paste(prev_months[2], "-", prev_year, sep=""), paste(prev_months[3], "-", prev_year, sep=""))
} else if(as.integer(read_month)==1) {
    prev_months<-c("12", "11", "10")
    prev_date1<-c(prev_date1, paste(prev_year, prev_months[1], "01", sep=""), paste(prev_year, prev_months[2], "01", sep=""), paste(prev_year, prev_months[3], "01", sep=""))
    prev_date2<-c(prev_date2, paste(prev_year, prev_months[1], as.character(gen_day2(prev_months[1], prev_year)), sep=""), paste(prev_year, prev_months[2], as.character(gen_day2(prev_months[2], prev_year)), sep=""), paste(prev_year, prev_months[3], as.character(gen_day2(prev_months[3], prev_year)), sep=""))
    mons_years<-c(mons_years, paste(prev_months[1], "-", prev_year, sep=""), paste(prev_months[2], "-", prev_year, sep=""), paste(prev_months[3], "-", prev_year, sep=""))
}
prev_date1<-c(prev_date1, paste(prev_year, read_month, "01", sep=""))
prev_date2<-c(prev_date2, paste(prev_year, read_month, as.character(gen_day2(read_month, prev_year)), sep=""))
mons_years<-c(mons_years, paste(read_month, "-", prev_year, sep=""))

#vectors of all date1 and date2 values
dates1<-c(date1, prev_date1)
dates2<-c(date2, prev_date2)



#Creating a file
date<-as.Date(date2, format="%Y%m%d")
date<-as.POSIXlt(date)

x<-readLines("report-style.css", encoding="UTF-8-BOM", warn=F)
x<-str_c(x, sep="\n", collapse="")
headerInFile<-paste("<html><meta charset='utf-8'><style>", x, "</style><title>Сводка данных статистики по сайту</title><h1>Сводка статистики для ", counters_data[read_num,][,'site'], " за ", date$mon+1,"-й месяц ", date$year-100+2000, " года</h1>", sep="")

filename<-paste(date$mon+1, " - ", date$year-100+2000, " report [", counters_data[read_num,][,"site"], "].html", sep="")
if(!file.exists(filename)) {
    writeLines(text=headerInFile, con=filename)
} else {
    print("This report has already been prepared.")
}



#Getting data

#traffic summary :  /stat/traffic/summary
write("<h2>Сводка посещаемости</h2>", file=filename, append=T)

traffic_summary<-data.frame(item=c("Посетителей за месяц:","Они совершили визитов:","Они просмотрели страниц:"), stringsAsFactors=F)
col_names<-c("Показатель")
for (i in 1:length(dates1)) {
    req<-paste("http://api-metrika.yandex.ru/stat/traffic/summary.json?id=", counterID, "&date1=", dates1[i], "&date2=", dates2[i], "&group=month&oauth_token=", appToken, sep="")
    read_traffic_summary<-fromJSON(txt=req)
    traffic_summary[[i+1]]<-c(read_traffic_summary$totals$visitors, read_traffic_summary$totals$visits, read_traffic_summary$totals$page_views)
    col_names<-c(col_names, paste("Значение за ", mons_years[i], sep=""))
}
colnames(traffic_summary)<-col_names
print(traffic_summary)

write("<table>", file=filename, append=T)
write("<tr>", file=filename, append=T)
for (bycols in 1:ncol(traffic_summary)) {
    cat("<th>", col_names[bycols], "</th>", sep="", file=filename, append=T)
}
write("</tr>", file=filename, append=T)
difference<-matrix(nrow=nrow(traffic_summary), ncol=ncol(traffic_summary)-2)
for (byrows in 1:nrow(traffic_summary)) {
    write("<tr>", file=filename, append=T)
    
    for (bycols in 1:ncol(traffic_summary)) {
        
        if (bycols == 1 | bycols == 2) {
            cat("<td>", traffic_summary[byrows,][,bycols], "</td>", sep="", file=filename, append=T)
        } else {
            difference[byrows,bycols-2] = as.integer(traffic_summary[byrows,][,2]) - as.integer(traffic_summary[byrows,][,bycols])
            if (difference[byrows,bycols-2] >= 0 ) {
                cat("<td class='colored-green'>", traffic_summary[byrows,][,bycols], " (+", difference[byrows,bycols-2], ")</td>", sep="", file=filename, append=T)
            } else {
                cat("<td class='colored-red'>", traffic_summary[byrows,][,bycols], " (", difference[byrows,bycols-2], ")</td>", sep="", file=filename, append=T)
            }
        }
        
    }
    
    write("</tr>", file=filename, append=T)
}
write("</table>", file=filename, append=T)



#sources summary :  /stat/sources/summary
write("<br><h2>Источники визитов</h2><h3>Сводка по источникам визитов</h3>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/stat/sources/summary.json?id=", counterID, "&date1=", date1, "&date2=", date2, "&group=month&oauth_token=", appToken, sep="")
sources_summary<-fromJSON(txt=req)
sources_summary<-sources_summary[["data"]]
sources_summary_visits<-data.frame(Source=sources_summary$name, stringsAsFactors=F)
sources_summary_visits$Visits<-sources_summary$visits
colnames(sources_summary_visits)<-c("Тип источника", "Визиты")
sources_summary_colnames<-colnames(sources_summary_visits)

write("<table>", file=filename, append=T)
cat("<tr><th>", sources_summary_colnames[1], "</th><th>", sources_summary_colnames[2], "</th></tr>", file=filename, sep="", append=T)
for(byrows in 1:nrow(sources_summary_visits)) {
    cat("<tr><td>", sources_summary_visits[byrows,][,1], "</td><td>", as.character(sources_summary_visits[byrows,][,2]), "</td></tr>", file=filename, sep="", append=T)
}
write("</table>", file=filename, append=T)


#sources sites :  /stat/sources/sites
write("<br><h3>Топ-15 сайтов источников переходов по ссылкам</h3><p>Некоторые сайты упоминаются в статистике несколько раз.</p>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/stat/sources/sites.json?id=", counterID, "&date1=", date1, "&date2=", date2, "&table_mode=plain&per_page=15&oauth_token=", appToken, sep="")
sources_sites<-fromJSON(txt=req)
sources_sites<-sources_sites[["data"]]
sources_sites_visits<-data.frame(Source=sources_sites$url, stringsAsFactors=F)
sources_sites_visits$Visits<-sources_sites$visits
colnames(sources_sites_visits)<-c("Адрес сайта", "Визиты")
sources_sites_colnames<-colnames(sources_sites_visits)

write("<table>", file=filename, append=T)
cat("<tr><th>", sources_sites_colnames[1], "</th><th>", sources_sites_colnames[2], "</th></tr>", file=filename, sep="", append=T)
for(by_rows in 1:nrow(sources_sites_visits)) {
    cat("<tr><td>", sources_sites_visits[by_rows,][,1], "</td><td>", as.character(sources_sites_visits[by_rows,][,2]), "</td></tr>", file=filename, sep="", append=T)
}
write("</table>", file=filename, append=T)


#sources - search phrases :  /stat/sources/phrases
write("<br><h3>Топ-30 поисковых фраз</h3>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/stat/sources/phrases.json?id=", counterID, "&date1=", date1, "&date2=", date2, "&table_mode=plain&per_page=30&oauth_token=", appToken, sep="")
sources_phrases<-fromJSON(txt=req)
sources_phrases<-sources_phrases[["data"]]
sources_phrases_visits<-data.frame(Source=sources_phrases$phrase, stringsAsFactors=F)
sources_phrases_visits$Visits<-sources_phrases$visits
colnames(sources_phrases_visits)<-c("Поисковая фраза", "Визиты")
sources_phrases_colnames<-colnames(sources_phrases_visits)

write("<table>", file=filename, append=T)
cat("<tr><th>", sources_phrases_colnames[1], "</th><th>", sources_phrases_colnames[2], "</th></tr>", file=filename, sep="", append=T)
for(by_rows in 1:nrow(sources_phrases_visits)) {
    cat("<tr><td>", sources_phrases_visits[by_rows,][,1], "</td><td>", as.character(sources_phrases_visits[by_rows,][,2]), "</td></tr>", file=filename, sep="", append=T)
}
write("</table>", file=filename, append=T)


#sources advertisement : /stat/sources/marketing
write("<br><h3>Рекламные системы</h3>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/stat/sources/marketing.json?id=", counterID, "&date1=", date1, "&date2=", date2, "&table_mode=plain&per_page=100&oauth_token=", appToken, sep="")
sources_marketing<-fromJSON(txt=req)
sources_marketing<-sources_marketing[["data"]]

if(length(sources_marketing)!=0) {
    sources_marketing_summary<-data.frame(advsystem=sources_marketing$name, stringsAsFactors=F)
    sources_marketing_summary$visits<-sources_marketing$visits
    sources_marketing_summary$depth<-sources_marketing$depth
    sources_marketing_summary$depth_for_site<-read_traffic_summary$totals$depth
    sources_marketing_summary$visit_time<-paste(sources_marketing$visit_time%/%60, ":", sources_marketing$visit_time%%60, sep="")
    sources_marketing_summary$visit_time_for_site<-paste(read_traffic_summary$totals$visit_time%/%60, ":", read_traffic_summary$totals$visit_time%%60, sep="")
    sources_marketing_summary$deny<-sources_marketing$denial
    sources_marketing_summary$denial_for_site<-read_traffic_summary$totals$denial
    colnames(sources_marketing_summary)<-c("Рекламная система", "Визиты", "Средняя глубина", "Ср. глуб. для всего сайта", "Среднее время", "Ср. вр. для всего сайта", "Отказы", "Отказы для всего сайта")
    sources_marketing_colnames<-colnames(sources_marketing_summary)
    
    write("<table>", file=filename, append=T)
    write("<tr>", file=filename, append=T)
    for (bycols in 1:ncol(sources_marketing_summary)) {
        cat("<th>", sources_marketing_colnames[bycols], "</th>", file=filename, sep="", append=T)
    }
    write("</tr>", file=filename, append=T)
    
    for(by_rows in 1:nrow(sources_marketing_summary)) {
        write("<tr>", file=filename, append=T)
        for (by_cols in 1:ncol(sources_marketing_summary)) {
            cat("<td>", sources_marketing_summary[by_rows,][,by_cols], "</td>", file=filename, sep="", append=T)
        }
        write("</tr>", file=filename, append=T)
    }
    write("</table>", file=filename, append=T)
} else {
    write("<p>В этом месяце реклама не проводилась.</p>", file=filename, append=T)
}



#goals

#get information about targets : /counter/{id}/goals
write("<br><h2>Маркетинговые цели для сайта</h2>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/counter/", counterID, "/goals.json?id=", counterID, "&oauth_token=", appToken, sep="")
counter_goals<-fromJSON(txt=req)
counter_goals<-counter_goals$goals

#data frame for goals
if(length(counter_goals)!=0) {
    write("<table>", file=filename, append=T)
    cat("<tr><th>Цель</th><th>Источник</th><th>Визитов</th><th>Конверсия</th><th>Достигнуто</th></tr>", file=filename, sep="", append=T)
    for (byrows in 1:nrow(counter_goals)){
        
        #traffic summary for goals (/stat/traffic/summary  &goal_id)
        cat("<tr><td><b>", counter_goals[byrows,][,"name"], "</b></td><td><i>По всем источникам</i></td>", file=filename, sep="", append=T)
        
        req<-paste("http://api-metrika.yandex.ru/stat/traffic/summary.json?id=", counterID, "&goal_id=", counter_goals[byrows,][,"id"], "&date1=", date1, "&date2=", date2, "&group=month&oauth_token=", appToken, sep="")
        traffic_summary_for_goal<-fromJSON(txt=req)	
        cat("<td>", traffic_summary_for_goal$totals$goal_reaches, "</td><td>", traffic_summary_for_goal$totals$conversion, "</td><td>", traffic_summary_for_goal$totals$visits, "</td>", file=filename, sep="", append=T)
        
        write("</tr>", file=filename, append=T)
        
        
        #adding information by sources (/stat/sources/summary  &goal_id)	
        req<-paste("http://api-metrika.yandex.ru/stat/sources/summary.json?id=", counterID, "&goal_id=", counter_goals[byrows,][,"id"], "&date1=", date1, "&date2=", date2, "&oauth_token=", appToken, sep="")
        sources_summary_for_goal<-fromJSON(txt=req)
        sources_summary_for_goal<-sources_summary_for_goal$data
        
        if(length(sources_summary_for_goal)!=0) {
            for (i in 1:nrow(sources_summary_for_goal)){
                write("<tr><td>&nbsp;</td>", file=filename, sep="", append=T)
                cat("<td>", sources_summary_for_goal[i,][,"name"], "</td><td>", sources_summary_for_goal[i,][,"goal_reaches"], "</td><td>",sources_summary_for_goal[i,][,"conversion"], "</td><td>",sources_summary_for_goal[i,][,"visits"], "</td>", sep="", file=filename, append=T)
                write("</tr>", file=filename, append=T)
            }
        } else {
            write("<tr><td>&nbsp;</td>", file=filename, sep="", append=T)
            cat("<td>нет информации</td><td>нет информации</td><td>нет информации</td><td>нет информации</td><td>", file=filename, append=T)
            write("</tr>", file=filename, append=T)
            
        }
        
    }
    write("</table>", file=filename, append=T)
} else {
    write("<p>В этом месяце цели не были указаны.</p>", file=filename, append=T)
}



#content
#popular content :  /stat/content/popular  &table_mode=plain  &per_page=20
write("<br><h2>Содержание</h2><h3>Топ-20 популярных по числу просмотров страниц</h3>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/stat/content/popular.json?id=", counterID, "&date1=", date1, "&date2=", date2, "&table_mode=plain&per_page=20&oauth_token=", appToken, sep="")
popular_content<-fromJSON(txt=req)
popular_content<-popular_content$data

popular_content_views<-data.frame(url=popular_content$url, stringsAsFactors=F)
popular_content_views$views<-popular_content$page_views
colnames(popular_content_views)<-c("Страница", "Просмотры")
popular_content_colnames<-colnames(popular_content_views)
write("<table>", file=filename, append=T)
cat("<tr><th>", popular_content_colnames[1], "</th><th>", popular_content_colnames[2], "</th></tr>", file=filename, sep="", append=T)

ful_url<-paste("http://", counters_data[read_num,][,"site"], sep="")
for(by_rows in 1:nrow(popular_content_views)) {
    cat("<tr><td>", str_replace(popular_content_views[by_rows,][,1], ful_url, ""), "</td><td>", as.character(popular_content_views[by_rows,][,2]), "</td></tr>", file=filename, sep="", append=T)
}
write("</table>", file=filename, append=T)

#entrances :  /stat/content/entrance  &table_mode=plain &per_page=20
write("<br><h3>Топ-20 страниц входа</h3>", file=filename, append=T)
req<-paste("http://api-metrika.yandex.ru/stat/content/entrance.json?id=", counterID, "&date1=", date1, "&date2=", date2, "&table_mode=plain&per_page=20&oauth_token=", appToken, sep="")
content_entrance<-fromJSON(txt=req)
content_entrance<-content_entrance$data

content_entrance_views<-data.frame(url=content_entrance$url, stringsAsFactors=F)
content_entrance_views$views<-content_entrance$page_views
colnames(content_entrance_views)<-c("Страница", "Просмотры")
content_entrance_colnames<-colnames(content_entrance_views)

write("<table>", file=filename, append=T)
cat("<tr><th>", content_entrance_colnames[1], "</th><th>", content_entrance_colnames[2], "</th></tr>", file=filename, sep="", append=T)
for(by_rows in 1:nrow(content_entrance_views)) {
    cat("<tr><td>", str_replace(content_entrance_views[by_rows,][,1], "http://mebelite.su", ""), "</td><td>", as.character(content_entrance_views[by_rows,][,2]), "</td></tr>", file=filename, sep="", append=T)
}
write("</table>", file=filename, append=T)

#end of file
write("<p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p><p>Отчет сгенерирован с использованием API Яндекс.Метрика.</p><p>&nbsp;</p></html>", file=filename, append=T)
