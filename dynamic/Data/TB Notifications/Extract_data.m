clear all; 

C = readtable('TB_notifications_2022-03-11.csv');

colnames = C.Properties.VariableNames;

% --- Filter on 2019 year
rows    = find(C.year==2019);
C1      = C(rows,:);

% --- Pull out the relevant notification data
notifcols  = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc'))];
C2         = C1(:,notifcols);
C2.allnoti = C2.ret_nrel + C2.c_newinc;

% --- Also get the TPT data
rows    = find(C.year==2020);
% C3      = C(rows,:);
TPTcols = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'hiv_elig_new_tpt')), find(strcmp(colnames,'hiv_elig_all'))];
C3      = C(rows,TPTcols);
vec     = C3.hiv_elig_new_tpt./C3.hiv_elig_all; vec(isnan(vec)) = 0;
C3.pTPT = vec;

% --- Prepare for saving
notifs     = C2(:,[1,end]);
notifs_new = C2(:,[1,3]);
pTPT       = C3(:,[1,end]); 
save notif_data notifs notifs_new pTPT;




% --- Also, get information for numbers diagnosed with HIV
col = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc')), find(strcmp(colnames,'hivtest_pos'))];
C4 = C1(:,notifcols);