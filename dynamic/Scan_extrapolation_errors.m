clear all; 

% iso3s_todo_control = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
%     'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','UKR','VNM','ZWE'};

% iso3s_todo_control = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
%     'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','UKR','VNM','ZWE'};

iso3s_todo_control = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','UKR','VNM','ZWE'};

for iiso3 = 1:length(iso3s_todo_control)
    iso3 = iso3s_todo_control{iiso3};
    load([iso3,'/extrapolated_outputs2024_simple.mat'],'prevestims_mid','opts','all_pct_yr2');
    
    newsim = squeeze(all_pct_yr2(2,1:4,2,:))';
    
    devi = 1 - newsim./prevestims_mid;
    maxdevi(iiso3) = max(abs(devi(:)));
end

figure; 
bar(maxdevi);
set(gca,'XTick',1:length(maxdevi),'XTickLabel',iso3s_todo_control);

inds = find(maxdevi>0.05);
iso3s_todo_control(inds)


newsim = squeeze(all_pct_yr2(2,:,2,:))';
