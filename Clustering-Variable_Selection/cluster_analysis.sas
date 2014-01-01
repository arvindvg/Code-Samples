/* This SAS macro is a multi utility macro that can be used for variable clustering, variable reductions, finding competitive merchants & finding best predictors for post-marketing analysis. 
 This macro also uses a %inform macro that calculates information value for each predictor variables against the dependent variables. This part was not written by me and is not included in this
 
 The input dataset contains a list of predictor variables, dependent variable, clustering parameters & a output dataset */

%macro cluster(ifile, vlist, dv, p, ofile) / parmbuff;
   %put syspbuff= &syspbuff.;
   /* ifile -- input file for variable clustering */
   /* vlist -- variable list used for the clustering */
   /* dv - Dependent variable */
   /* p --     percent of variance to be explained by the first cluster - Test out with different option here to see how it performs */
   /* ofile -- output file contains results of clustering, correlations, stepdisc, and IV */

   data trn_f;
      set &ifile.;
          keep &dv. &vlist.;
   run;

   ods listing close;
   ods output clusterquality=summary
              rsquare(match_all) = clusters;

      proc varclus data=trn_f percent=&p. short hi outtree=temptree;
         var &vlist.;
      run;
   ods listing;

   data _null_;
      set summary;
      call symput('ncl', trim(left(numberofclusters-2)));
   run;

   proc print data=clusters&ncl.;
   run;

   data clusters&ncl.(drop=temp);
      set clusters&ncl.;

      if compress(cluster)^='' then temp=cluster;
      else cluster=temp;
          retain temp;
   run;

   /* RUNNING A STEPWISE DISCRIMINANT ANALYIS */
   *ods trace on;
   ods listing close;
   ods output steps=SDiscrim;

      proc stepdisc data=trn_f method=stepwise maxstep=1 pr2entry=0.0005 pr2stay=0.001;
         class &dv.;
         var &vlist.;
      run;
   ods listing;

   data SDiscrim(keep= variable StepDisc_RSq StepDisc_ProbF);
      set SDiscrim;
          rename RSquare= StepDisc_RSq;
          rename ProbF=   StepDisc_ProbF;
   run;

   proc sort data=sdiscrim;
      by variable;
   run;

   /* CALCULATING SPEARMAN CORRELATIONS WITH THE DEPENDENT VARIABLE */
   *ods trace on;
   ods listing close;
   ods output SpearmanCorr=Corr_Spearman;

      proc corr data=trn_f spearman nosimple;
         var &dv.;
         with &vlist.;
      run;
   ods listing;

   data Corr_Spearman;
      set Corr_Spearman;
          rename &dv.=  DV_Spearman_Corr;
          rename p&dv.= Pval_Spearman;
   run;

   proc sort data=Corr_Spearman;
      by variable;
   run;

   data ranking_metrics;
      merge SDiscrim(in=a) Corr_Spearman(in=b);
          by variable;
   run;

   proc datasets library=WORK;
      delete SDiscrim Corr_Spearman;
   run;

   proc sort data=ranking_metrics;
      by variable;
   run;

   proc sort data=clusters&ncl.(drop=ControlVar);
      by variable;
   run;

   data clusters&ncl.;
      merge clusters&ncl.(in=a) ranking_metrics(in=b);
          by variable;
   run;

   proc contents data=clusters&ncl out=len noprint;
   run;

   data _NULL_;
      set len(where=(upcase(name)='VARIABLE'));
      call symput('max_len', left(length));
   run;
   %put MAX VARIABLE NAME LENGTH IS &max_len.;
   /*%let cat=prod_type acct_zip_cd;*/
   /* NO CATEGORICAL VARIABLES ARE SUPPLIED, ALL ARE CONTINUOUS */
   %inform(trn_f, , &vlist., &max_len., &dv., IV, groups=15);

   data clusters&ncl.;
      set clusters&ncl.;
      variable= upcase(variable);
   run;

   data IV;
      set IV;
      variable= upcase(variable);
   run;

   proc sort data=clusters&ncl.;
      by variable;
   run;

   proc sort data=IV;
      by variable;
   run;

   data &ofile.;
      merge clusters&ncl.(in=a) IV(in=b);
      by variable;

      format c2 4.0;
      c2 = substr(left(compress(cluster)), 8);
   run;

   proc sort data=&ofile.;
      by c2 descending IV RSquareRatio;
   run;

   data &ofile;
      set &ofile.;
         *drop c2;
          rename RSquareRatio=_1_RSquareRatio;
   run;

   proc sql;
      create table &ofile. as
      select C2, Cluster, Variable, OwnCluster, NextClosest, _1_RSquareRatio, StepDisc_RSq,
             StepDisc_ProbF, DV_Spearman_Corr, Pval_Spearman, IV, rank_IV,
             ML, prop_use, chisq, P_VALUE, rank_ml, rank_P, rank_chi /*label*/
          from &ofile.;
   quit;


   proc print data=&ofile.;
      title1 "*** Variables sorted by Cluster, IV, & RSquareRatio ***";
   run;

%mend cluster;
/* ********************************************************************************** */

options nocenter;
*options ps = 1000
        nolabel;

libname dnm '/main/u20/vaedata/users/vaeaven/campaigns/dnm/phase2';


%let ilist1= c1_0712 c2_0712 entropy fa1 fa2 fa3 fa4 fa5 fa6 fa7 fa8 fa9 fa10 fa11 fa12
             gini smcc_pct_b2b smcc_pct_direct_marketing smcc_pct_entertainment
                smcc_pct_health_care smcc_pct_household_service smcc_pct_organization smcc_pct_other
                smcc_pct_professional_service smcc_pct_restaurants smcc_pct_retail
                smcc_pct_schools smcc_pct_transportation smcc_pct_travel tot_tran_amt tot_tran_cnt
                tran_pct_offline tran_pct_online tran_pct_other tran_pct_phone;
*%let cat=;

data dnm.mailed_list;
set dnm.mailed_list;
if total_active gt 0 then activity=1;
else activity=0;
run;

/* Must be continous or indicator variables for PROC VARCLUS to run */
%cluster(dnm.mailed_list, &ilist1.,activity, 50, dnm.vc);
