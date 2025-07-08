files_clima <- list.files('data/climatology/',pattern='climatology',full.names = T)

for(i in 1:length(files_clima)){
  if(i == 1){
    df_clima <- read.csv(files_clima[i],sep=',') |> dplyr::mutate(
      local = stringr::str_to_upper(stringr::str_remove(files_clima[i],'data/climatology/climatology_')
                                    |> stringr::str_remove('.csv'))
    )
  }else{
    df_a <- read.csv(files_clima[i],sep=',') |> dplyr::mutate(
      local = stringr::str_to_upper(stringr::str_remove(files_clima[i],'data/climatology/climatology_')
                                    |> stringr::str_remove('.csv'))
    )
    df_clima <- rbind(df_clima,df_a)
  }
}


files_validation <-list.files('data/climatology/',pattern='tab',full.names = T)

for(i in 1:length(files_validation)){
  if(i == 1){
    df_val <- read.csv(files_validation[i],sep=',') |> dplyr::mutate(
      local = stringr::str_to_upper(stringr::str_remove(files_validation[i],'data/climatology/tab_validation_')
                                    |> stringr::str_remove('.csv'))
    )
  }else{
    df_a <- read.csv(files_validation[i],sep=',') |> dplyr::mutate(
      local = stringr::str_to_upper(stringr::str_remove(files_validation[i],'data/climatology/tab_validation_')
                                    |> stringr::str_remove('.csv'))
    )
    df_val <- rbind(df_val,df_a)
  }
}



df_val |>
  dplyr::right_join(
    df_clima |> dplyr::mutate(
      prec_cli=prec,
      temp_cli = temp,
      etp_cli = etp
      ) |>
      dplyr::select(month,local,prec_cli,temp_cli,etp_cli) |>
      dplyr::group_by(month,local) |>
      dplyr::summarise_all(mean)
    ) |>
  dplyr::mutate(
    prec_ano = prec-prec_cli,
    temp_ano = temp-temp_cli,
    etp_ano = etp-etp_cli
  ) |>
  dplyr::select(
    local,year,month,prec_ano,temp_ano,etp_ano
  ) |>
  dplyr::group_by(local,year,month) |>
  dplyr::summarise(
    prec= mean(prec_ano),
    temp=mean(temp_ano),
    etp=mean(temp_ano),
    prec_sd= sd(prec_ano),
    temp_sd=sd(temp_ano),
    etp_sd=sd(temp_ano)
  ) |>
  dplyr::mutate(
    date= lubridate::make_date(year,month,'15')
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=prec,
                               ymin=prec-prec_sd,
                               ymax=prec+prec_sd))+
  ggplot2::geom_line()+
  ggplot2::geom_ribbon(alpha=.3)+
  ggpubr::stat_cor()+
  ggplot2::facet_wrap(~local)+
  ggplot2::ylab('Precipitation (mm)')

ggplot2::ggsave('prec_ano.png',units="in", width=12, height=6,
                dpi=300)


df_val |>
  dplyr::right_join(
    df_clima |> dplyr::mutate(
      prec_cli=prec,
      temp_cli = temp,
      etp_cli = etp
    ) |>
      dplyr::select(month,local,prec_cli,temp_cli,etp_cli) |>
      dplyr::group_by(month,local) |>
      dplyr::summarise_all(mean)
  ) |>
  dplyr::mutate(
    prec_ano = prec-prec_cli,
    temp_ano = temp-temp_cli,
    etp_ano = etp-etp_cli
  ) |>
  dplyr::select(
    local,year,month,prec_ano,temp_ano,etp_ano
  ) |>
  dplyr::group_by(local,year,month) |>
  dplyr::summarise(
    prec= mean(prec_ano),
    temp=mean(temp_ano),
    etp=mean(temp_ano),
    prec_sd= sd(prec_ano),
    temp_sd=sd(temp_ano),
    etp_sd=sd(temp_ano)
  ) |>
  dplyr::mutate(
    date= lubridate::make_date(year,month,'15')
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=temp,
                               ymin=temp-temp_sd,
                               ymax=temp+temp_sd))+
  ggplot2::geom_line()+
  ggplot2::geom_ribbon(alpha=.3)+
  ggpubr::stat_cor()+
  ggplot2::facet_wrap(~local)+
  ggplot2::ylab('Temp (°C)')

ggplot2::ggsave('temp_ano.png',units="in", width=12, height=6,
                dpi=300)

df_val |>
  dplyr::right_join(
    df_clima |> dplyr::mutate(
      prec_cli=prec,
      temp_cli = temp,
      etp_cli = etp
    ) |>
      dplyr::select(month,local,prec_cli,temp_cli,etp_cli) |>
      dplyr::group_by(month,local) |>
      dplyr::summarise_all(mean)
  ) |>
  dplyr::mutate(
    prec_ano = prec-prec_cli,
    temp_ano = temp-temp_cli,
    etp_ano = etp-etp_cli
  ) |>
  dplyr::select(
    local,year,month,prec_ano,temp_ano,etp_ano
  ) |>
  dplyr::group_by(local,year,month) |>
  dplyr::summarise(
    prec= mean(prec_ano),
    temp=mean(temp_ano),
    etp=mean(temp_ano),
    prec_sd= sd(prec_ano),
    temp_sd=sd(temp_ano),
    etp_sd=sd(temp_ano)
  ) |>
  dplyr::mutate(
    date= lubridate::make_date(year,month,'15')
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=etp,
                               ymin=etp-etp_sd,
                               ymax=etp+etp_sd))+
  ggplot2::geom_line()+
  ggplot2::geom_ribbon(alpha=.3)+
  ggpubr::stat_cor()+
  ggplot2::facet_wrap(~local)


df_clima |>
  ggplot2::ggplot(ggplot2::aes(x= month, y= etp,group=month))+
  ggplot2::geom_boxplot()+
  ggplot2::facet_wrap(~local)+
  ggplot2::theme_bw()+
  ggplot2::labs(
    x= 'month',
    y= expression('ET'[p]~'(mm month'^-1~')')
  )
ggplot2::ggsave('etp_clim.png',units="in", width=12, height=6,
                dpi=300)



#####

df_clima |>
  dplyr::group_by(local,month) |>
  dplyr::summarise(
    et = mean(etp),
    et_sd = sd(etp),
    et_max = max(etp)
    ) |>
  dplyr::filter(local=='AMZ')

df_clima |>
  dplyr::group_by(local) |>
  dplyr::summarise(
    et = mean(etp),
    et_sd = sd(etp),
    et_max = max(etp),
    et_min = min(etp),
    cv = et_sd/et
  )
