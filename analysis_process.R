library(lme4)
# demographic characteristics statistic (disperse）#
group_test_disperse<-function(untwins_data,twins_data,data){
  income_data_com$demo_comb_income_v2_l<-as.numeric(income_data_com$demo_comb_income_v2_l)
  income_ran1<-income_data_com[which(income_data_com$demo_comb_income_v2_l<=5),]$subjectkey
  income_ran2<-income_data_com[which(income_data_com$demo_comb_income_v2_l==6),]$subjectkey
  income_ran3<-income_data_com[which(income_data_com$demo_comb_income_v2_l==7),]$subjectkey
  income_ran4<-income_data_com[which(income_data_com$demo_comb_income_v2_l==8),]$subjectkey
  income_ran5<-income_data_com[which(income_data_com$demo_comb_income_v2_l>=9),]$subjectkey
  unknow<-setdiff(union(untwins_data,twins_data),income_data_com$subjectkey)
  ## education group #
  edu_high_less<-ecucation_par_info[which(ecucation_par_info$demo_prnt_ed_v2_l<=12),]$subjectkey
  high_school<-ecucation_par_info[which(ecucation_par_info$demo_prnt_ed_v2_l>12&ecucation_par_info$demo_prnt_ed_v2_l<16),]$subjectkey
  associated_be<-ecucation_par_info[which(ecucation_par_info$demo_prnt_ed_v2_l>=16&ecucation_par_info$demo_prnt_ed_v2_l<18),]$subjectkey
  Bachelor<-ecucation_par_info[which(ecucation_par_info$demo_prnt_ed_v2_l>=18&ecucation_par_info$demo_prnt_ed_v2_l<19),]$subjectkey
  master_all<-ecucation_par_info[which(ecucation_par_info$demo_prnt_ed_v2_l>=19),]$subjectkey
  unknow<-setdiff(union(untwins_data,twins_data),ecucation_par_info$subjectkey)
  #edacuation #
  a_1<-intersect(edu_high_less,untwins_data)%>%length()
  a_2<-intersect(high_school,untwins_data)%>%length()
  a_3<-intersect(associated_be,untwins_data)%>%length()
  a_4<-intersect(Bachelor,untwins_data)%>%length()
  a_5<-intersect(master_all,untwins_data)%>%length()
  a_6<-intersect(unknow,untwins_data)%>%length()
  b_1<-intersect(edu_high_less,twins_data)%>%length()
  b_2<-intersect(high_school,twins_data)%>%length()
  b_3<-intersect(associated_be,twins_data)%>%length()
  b_4<-intersect(Bachelor,twins_data)%>%length()
  b_5<-intersect(master_all,twins_data)%>%length()
  b_6<-intersect(unknow,twins_data)%>%length()
  data<-rbind(cbind(a_1,a_2,a_3,a_4,a_5,a_6),cbind(b_1,b_2,b_3,b_4,b_5,b_6))
  # chisq.test(data)
  data<-birth_weight
  rownames(data)<-data$subjectkey
  ##
  one_group<-data[untwins_data,]$V7
  sec_group<-data[twins_data,]$V7
  a<-one_group[which(one_group=="0")]%>%length()
  b<-one_group[which(one_group=="1")]%>%length()
  c<-sec_group[which(sec_group=="0")]%>%length()
  d<-sec_group[which(sec_group=="1")]%>%length()
  data<-rbind(cbind(a,b),cbind(c,d))
  chisq.test(data)
  one_group<-as.numeric(one_group)
  sec_group<-as.numeric(sec_group)
  one_group<-one_group[which(!is.na(one_group))]
  sec_group<-sec_group[which(!is.na(sec_group))]
  ###########
  print(mean(one_group))
  print(sd(one_group))
  print(mean(sec_group))
  print(sd(sec_group))
  print(t.test(one_group,sec_group))
}
# for continuous variables
group_test_continue<-function(untwins_data,twins_data,data){
  single_info<-data[untwins_data,]
  twins_info<-data[twins_data,]
  print(t.test(single_info,twins_info))
  
}
#mediation function
getmedre_vm2wmhv <- function(datafram_data){
  # datafram_data<-glm_dataf
  Data_X <- datafram_data[,c(2:ncol(datafram_data))]
  Data_Y <- datafram_data
  fitM <- glm(structure_info ~ .,data = Data_X)
  fitY <- glm(function_socre ~ .,data = Data_Y)
  fitMed <- mediate(fitM, fitY, treat="group_diff", mediator="structure_info",sims = 10000)
  
  sum_fitM <- summary(fitM)  
  sum_fitY <- summary(fitY)
  ###
  ##
  re_beta <- c(sum_fitM$coefficients[2,1],sum_fitY$coefficients[3,1],fitMed$z0,fitMed$d0,fitMed$tau.coef)
  re_ci_m <- c(sum_fitM$coefficients[2,1]-1.96*sum_fitM$coefficients[2,1],
               sum_fitY$coefficients[3,1]-1.96*sum_fitY$coefficients[3,1],
               unname(fitMed$z0.ci[1]),
               unname(fitMed$d0.ci[1]),unname(fitMed$tau.ci[1]))
  re_ci_p<- c(sum_fitM$coefficients[2,1]+1.96*sum_fitM$coefficients[2,1],
              sum_fitY$coefficients[3,1]+1.96*sum_fitY$coefficients[3,1],
              unname(fitMed$z0.ci[2]),
              unname(fitMed$d0.ci[2]),unname(fitMed$tau.ci[2]))
  re_pval <- c(sum_fitM$coefficients[2,4],sum_fitY$coefficients[3,4],fitMed$z0.p,fitMed$d0.p,fitMed$tau.p)
  ##value set ##
  return(rbind(re_beta,re_ci_m,re_ci_p,re_pval))
}

media_computer<-function(function_socre,structure_info){

  glm_dataf<-data.frame(function_socre,group_diff,structure_info,income_var,sex_var,age_var,
                        age_var_father,BMI_var,ges_var,Puberty_var,weight_var,educa_var,
                        age_var_mother,mother_marijuana_var,site_info,mother_morphine_var,mother_var_smo,mother_var_alcho)
  glm_dataf<-na.omit(glm_dataf)
  ###
  med_res<-getmedre_vm2wmhv(glm_dataf)
  return(med_res)
  
}
## linear regression process
compute_T_value<-function(data,untwins_data,twins_data,task_name,cog_cbcl){
  #data<-light_id
  library(compute.es)
  # ,structure_feature,cog_index
  #data<-feature_info
  # data<-cbcl_socre_base
  # data<-area_all
  single_data<-untwins_data
  twins_data<-twins_data
  # single_data<-normal_single
  # twins_data<-normal_twins
  #data<-cognitive_baseline[,cog_sig_chose$name_cbcl.sig_data.]
  #structure_feature<-feature_info[,mri_01$name_cbcl.sig_data.]
  # Covariate single
  #sinle_data
  income_data_com_single<-income_data_com[single_data,]$demo_comb_income_v2_l%>%as.numeric()
  age_info_father_single<-age_info_father[single_data,]$devhx_4_p%>%as.numeric()
  age_info_mother_single<-age_info_mother[single_data,]$devhx_3_p%>%as.numeric()
  mother_smoke_single<-mother_smoke[single_data,]$devhx_9_tobacco%>%as.numeric()
  mother_drink_single<-mother_alcohol[single_data,]$devhx_9_alcohol%>%as.numeric()
  mother_marijuana_sinlge<-mother_marijuana[single_data,]$devhx_9_marijuana%>%as.numeric()
  mother_morphine_single<-mother_morphine[single_data,]$devhx_9_her_morph%>%as.numeric()
  ges_single<-ges_allsample[single_data,]$ges_week_0_all%>%as.numeric()
  BMI_single<-physics_BMI[single_data,]$BMI%>%as.numeric()
  Puberty_single<-all_puber_info[single_data,]$all_puber%>%as.numeric()
  educa_single<-ecucation_par_info[single_data,]$ecucation_par_all%>%as.numeric()
  weight_single<-birth_weight[single_data,]$V5%>%as.numeric()
  sex_single<-twins_info[single_data,]$sex%>%as.numeric()
  age_single<-twins_info[single_data,]$interview_age%>%as.numeric()
  single_site<-all_sitealsub[single_data,]
  single_race<-race_info[single_data,]
  
  ##twins data ##
  income_data_com_twins<-income_data_com[twins_data,]$demo_comb_income_v2_l%>%as.numeric()
  age_info_father_twins<-age_info_father[twins_data,]$devhx_4_p%>%as.numeric()
  age_info_mother_twins<-age_info_mother[twins_data,]$devhx_3_p%>%as.numeric()
  mother_smoke_twins<-mother_smoke[twins_data,]$devhx_9_tobacco%>%as.numeric()
  mother_drink_twins<-mother_alcohol[twins_data,]$devhx_9_alcohol%>%as.numeric()
  mother_marijuana_twins<-mother_marijuana[twins_data,]$devhx_9_marijuana%>%as.numeric()
  mother_morphine_twins<-mother_morphine[twins_data,]$devhx_9_her_morph%>%as.numeric()
  ges_twins<-ges_allsample[twins_data,]$ges_week_0_all%>%as.numeric()
  BMI_twins<-physics_BMI[twins_data,]$BMI%>%as.numeric()
  Puberty_twins<-all_puber_info[twins_data,]$all_puber%>%as.numeric()
  educa_twins<-ecucation_par_info[twins_data,]$ecucation_par_all%>%as.numeric()
  weight_twins<-birth_weight[twins_data,]$V5%>%as.numeric()
  sex_twins<-twins_info[twins_data,]$sex%>%as.numeric()
  age_twins<-twins_info[twins_data,]$interview_age%>%as.numeric()
  twin_site<-all_sitealsub[twins_data,]
  twin_race<-race_info[single_data,]
  income_var<-c(income_data_com_single,income_data_com_twins)
  age_var_father<-c(age_info_father_single,age_info_father_twins)
  age_var_mother<-c(age_info_mother_single,age_info_father_twins)
  mother_var_smo<-c(mother_smoke_single,mother_smoke_twins)
  mother_var_alcho<-c(mother_drink_single,mother_drink_twins)
  mother_marijuana_var<-c(mother_marijuana_sinlge,mother_marijuana_twins)
  mother_morphine_var<-c(mother_morphine_single,mother_morphine_twins)
  BMI_var<-c(BMI_single,BMI_twins)
  ges_var<-c(ges_single,ges_twins)
  Puberty_var<-c(Puberty_single,Puberty_twins)
  weight_var<-c(weight_single,weight_twins)
  educa_var<-c(educa_single,educa_twins)
  sex_var<-c(sex_single,sex_twins)
  age_var<-c(age_single,age_twins)
  site_info<-c(single_site,twin_site)
  group_diff<-c(rep(0,length(single_data)),rep(1,length(twins_data)))
  race_info<-c(single_race,twin_race)
  ####
  #print(length(twins_data))
  name_cbcl<-colnames(data)
  sig_data<-vector()
  p_values_sig<-vector()
  sample_num<-vector()
  single_mean_d<-vector()
  twins_mean_d<-vector()
  t_value<-vector()
  cohen_d<-vector()
  ## 11:89
  save_list<-list()
  ##18:106
  library(lmerTest)
  library(sjstats)
  if(cog_cbcl=="CBCL"){
    start_n=10
    seq_dep=4
  }else if(cog_cbcl=="cog"){
    start_n=10 
    seq_dep=1
  }else if(cog_cbcl=="struct") {
    start_n=1
    seq_dep=1
  }
    for (i in seq(from=start_n,to=ncol(data),seq_dep)) {
      cbcl_socre_single<-data[single_data,i]%>%as.numeric()
      cbcl_socre_twins<-data[twins_data,i]%>%as.numeric()
      ###
      if(length(cbcl_socre_single[which(is.na(cbcl_socre_single))])<(2*(length(cbcl_socre_single))/3))
      {
        # print(i)
        cbcl_var_cat<-c(cbcl_socre_single,cbcl_socre_twins)
        ##the mean and the std
        sinlle_mean<-mean(cbcl_socre_single[which(!is.na(cbcl_socre_single))])%>%round(digits = 3)
        sinlle_std<-sd(cbcl_socre_single[which(!is.na(cbcl_socre_single))])%>%round(digits = 3)
        sinlle_mean_co<-paste(as.character(sinlle_mean),"(",as.character(sinlle_std),")",sep = "")
        ##
        twins_mean<-mean(cbcl_socre_twins[which(!is.na(cbcl_socre_twins))])%>%round(digits = 3)
        twins_std<-sd(cbcl_socre_twins[which(!is.na(cbcl_socre_twins))])%>%round(digits = 3)
        twins_mean_co<-paste(as.character(twins_mean),"(",as.character(twins_std),")",sep = "")
        ### get the glm datafram ges_var 
        glm_dataf<-data.frame(cbcl_var_cat,group_diff,income_var,sex_var,age_var,site_info,
                              age_var_father,BMI_var,ges_var,Puberty_var,weight_var,educa_var,race_info,
                              age_var_mother,mother_morphine_var,mother_marijuana_var,mother_var_smo,mother_var_alcho)
        rownames(glm_dataf)<-c(single_data,twins_data)
        prs_id<-intersect(rownames(adhd_prs),rownames(asd_prs))
        glm_dataf<-glm_dataf[prs_id,]#
        glm_dataf<-cbind(glm_dataf,adhd_prs,asd_prs,MDD_prs,SCZ_prs,BIP_prs)
        glm_dataf<-cbind(glm_dataf,adhd_prs,asd_prs,MDD_prs,SCZ_prs,BIP_prs)
        glm_dataf<-na.omit(glm_dataf)
        twins_data_us<-glm_dataf[which(glm_dataf$group_diff==1),]
        single_data_us<-glm_dataf[which(glm_dataf$group_diff==0),]
        lin.mod = lmer(cbcl_var_cat~group_diff+income_var+
                         sex_var+age_var+age_var_father+BMI_var+ges_var+Puberty_var+adhd_prs+asd_prs+race_info
                         weight_var+educa_var+age_var_mother+mother_marijuana_var+MDD_prs+SCZ_prs+BIP_prs+
                         mother_morphine_var+mother_var_smo+mother_var_alcho+(1|site_info),data=glm_dataf)
        pVal<-p_value(lin.mod)$p[2]
        tVal<-summary(lin.mod)$coefficients[2,4]
        if(nrow(twins_data_us)>3){
          print(name_cbcl[i])
          te_conhed<-tes(tVal, n.1=nrow(twins_data_us), n.2=nrow(single_data_us))
          cohen_d<-c(cohen_d,te_conhed$d)
          sample_num<-c(sample_num,nrow(glm_dataf))
          single_mean_d<-c(single_mean_d,sinlle_mean_co)
          twins_mean_d<-c(twins_mean_d,twins_mean_co)
          sig_data<-c(sig_data,i)
          print(pVal)
          p_values_sig<-c(p_values_sig,pVal)
          t_value<-c(t_value,tVal)
        }
      }
    }
    redata<-data.frame(name_cbcl[sig_data],twins_mean_d,single_mean_d,p_values_sig,t_value,cohen_d)
    redata$p_values_sig<-p.adjust(redata$p_values_sig,method = "fdr")
  return(redata)
}

