% Written in preparation for 2024 global TB report: look back at
% monthly/quarterly notification data in previous year's report, and
% compare with annual notifications that were subsequently reported. Which
% countries saw major changes?

clear all; 

% List all modelled countries
iso3s = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','IND','KAZ','KEN','KGZ','KHM','LSO','MEX','MMR','MNG','MYS','NEP','PAK','PER','PHL','THA','UKR','UZB','VNM','ZWE'};

load disruption_data_2023_04_24.mat;

tmp1 = sum(mdata(:,:,3),2,'omitnan');
tmp2 = sum(qdata(:,:,3),2,'omitnan');
allnoti = tmp1+tmp2;

% Get annual notification data for comparison
C = readtable('../TB Notifications/TB_notifications_2024-06-23.csv');
colnames = C.Properties.VariableNames;

% --- Filter on years from 2015
rows      = find(C.year==2022);
C1        = C(rows,:);
notifcols = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'c_newinc'))];
C2        = C1(:,notifcols);

for ii = 1:length(iso3s)
    iso3 = iso3s{ii};
    
    i1 = find(strcmp(iso3_disrp,iso3));
    if isempty(i1)
        notif1 = NaN;
    else
        notif1 = allnoti(i1);
    end

    i2 = find(strcmp(C2.iso3,iso3));
    notif2 = C2.c_newinc(i2);

    bothnotis(ii,:) = [notif1, notif2];
end

vec = bothnotis(:,2)./bothnotis(:,1)-1;
inds = find(vec>0.01);
iso3s(inds)
vec(inds)
