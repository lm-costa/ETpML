files_names <- list.files('data/',pattern='.csv', full.names = T)

for(i in 1:length(files_names)){
  if(i == 1){
    df <- read.csv(files_names[i],sep=';') |> dplyr::mutate(
      local = stringr::str_to_upper(stringr::str_remove(files_names[i],'data/treinamento_')
      |> stringr::str_remove('.csv'))
    )
  }else{
    df_a <- read.csv(files_names[i],sep=';') |> dplyr::mutate(
      local = stringr::str_to_upper(stringr::str_remove(files_names[i],'data/treinamento_')
      |> stringr::str_remove('.csv'))
    )
    df <- rbind(df,df_a)
    }
}

df |>
  ggplot2::ggplot()+
  ggplot2::geom_line(data=df,
                     ggplot2::aes(x=Epoch,y=RF,
                                  ymin=RF-RF_sd,
                                  ymax=RF+RF_sd,
                                  color='RF')
                     )+
  ggplot2::geom_ribbon(data=df,
                       ggplot2::aes(x=Epoch,y=RF,
                                    ymin=RF-RF_sd,
                                    ymax=RF+RF_sd,
                                    fill='RF'),
                       alpha=.3)+
  ggplot2::geom_line(data = df, ggplot2::aes(x=Epoch, y=MLP,
                                             ymin=MLP-MLP_sd,
                                             ymax=MLP+MLP_sd,
                                             color='MLP')
                     )+
  ggplot2::geom_ribbon(data = df, ggplot2::aes(x=Epoch, y=MLP,
                                               ymin=MLP-MLP_sd,
                                               ymax=MLP+MLP_sd,
                                               fill='MLP'),
                       alpha=.3)+
  ggplot2::geom_line(data = df, ggplot2::aes(x=Epoch, y=SW,
                                             ymin=SW-SW_sd,
                                             ymax=SW+SW_sd,
                                             color='SW')
                     )+
  ggplot2::geom_ribbon(data = df, ggplot2::aes(x=Epoch, y=SW,
                                               ymin=SW-SW_sd,
                                               ymax=SW+SW_sd,
                                               fill='SW'),
                       alpha=.3)+
  ggplot2::scale_color_manual(name='Model',
                              breaks=c('RF','MLP','SW'),
                              values=c('RF'='red', 'MLP'='blue', 'SW'='darkgreen'))+
  ggplot2::scale_fill_manual(name='Model',
                              breaks=c('RF','MLP','SW'),
                              values=c('RF'='red', 'MLP'='blue', 'SW'='darkgreen'))+
  ggplot2::facet_wrap(~local)+
  ggplot2::ylim(-0.5,1)+
  ggplot2::ylab(expression('R'^2))


ggplot2::ggsave('model_r2_score.png',units="in", width=10, height=5,
                dpi=300)


#######

files_names <- list.files('data/',pattern='xlsx', full.names = T)
df <- readxl::read_excel(files_names)


df |>
  ggplot2::ggplot(ggplot2::aes(
    x=year,y=as.numeric(R2)
  ))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::geom_smooth(method = 'lm',se=F)+
  ggpubr::stat_cor(label.y.npc = 'bottom',label.x.npc = 'left')+
  ggplot2::facet_wrap(~local,scales = 'fixed')+
  ggplot2::ylab(expression('R'^2))

ggplot2::ggsave('model_r2_forecast.png',units="in", width=12, height=6,
                dpi=300)


df |>
  dplyr::filter(local=='Cw/Cf') |>
  dplyr::pull(R2) |>
  as.numeric() |>
  na.omit() |>
  trend::mk.test()
