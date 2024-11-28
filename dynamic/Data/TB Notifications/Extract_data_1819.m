% 1819: Version to get data from both 2018 and 2019, for purpose of
% calculating trends

clear all; 

C = readtable('TB_notifications_2022-03-11.csv');

colnames = C.Properties.VariableNames;

% --- Filter on 2019 year
rows    = find(C.year==2019 | C.year==2018);
C1      = C(rows,:);

% --- Pull out the relevant notification data
notifcols  = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'country')), find(strcmp(colnames,'g_whoregion')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc'))];
C2         = C1(:,notifcols);
C2.allnoti = C2.ret_nrel + C2.c_newinc;

% --- Prepare for saving
notifs_18 = C2(1:2:end,[1:3,end]);
notifs_19 = C2(2:2:end,[1:3,end]);



% -------------------------------------------------------------------------
% --- Also, make lookups that will be useful in later categorisations -----

% --- Get a list of countries in each region
regs = unique(notifs_19.g_whoregion);
for ir = 1:length(regs)
    rows = find(strcmp(notifs_19.g_whoregion,regs{ir}));
    reg2iso3s.(regs{ir}) = notifs_19.iso3(rows);
end

% --- Get a lookup of each iso3 code to country names, in agreement with Thembisa model
iso3s = unique(notifs_19.iso3);
for ic = 1:length(iso3s)
    ctr = iso3s{ic};
    row = find(strcmp(notifs_19.iso3,ctr));
    iso2ctry.(ctr) = notifs_19.country{row};
end
% Make adjustments according to labelling in Thembisa model
iso2ctry.COD = 'Democratic Republic of the Congo';
iso2ctry.GBR = 'United Kingdom';


save notif_data_1819 notifs_18 notifs_19;
save lookups reg2iso3s iso2ctry;



return;

















reg = {};

% --- Populate regions for country ----------------------------------------
iso3s = notifs_18.iso3;
for ic = 1:length(iso3s)
    ctr = iso3s{ic};
    reg{ic} = notifs_18.g_whoregion{find(strcmp(notifs_18.iso3,ctr))};
end

lookup = [iso3s, reg'];
% Reverse, to get a list of countries in each region
regs = unique(reg);
for ir = 1:length(regs)
    ctrlist.(regs{ir}) = lookup(find(strcmp(lookup(:,2),regs{ir})),1);
end

% --- Get a lookup of each iso3 code to country ---------------------------
for ic = 1:length(ctrs2_mat)
    ctr = ctrs2_mat{ic};
    row = find(strcmp(notifs_18.iso3,ctr));
    iso2ctry.(ctr) = notifs_18.country{row};
end
% Make adjustments according to labelling in Thembisa model
iso2ctry.COD = 'Congo';
iso2ctry.GBR = 'United Kingdom';

save notif_data_1819 notifs_18 notifs_19;



% % --- Also, get information for numbers diagnosed with HIV
% col = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc')), find(strcmp(colnames,'hivtest_pos'))];
% C4 = C1(:,notifcols);