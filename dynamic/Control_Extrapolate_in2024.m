% Iterate over all countries to simulate disruptions

clear all;

iso3s_todo_control = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','UKR','VNM','ZWE'};

% iso3s_todo_control = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
%     'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','VNM','ZWE'};

% iso3s_todo_control = {'UKRiso3'};

% iso3s_todo_control = {'VNM','ZWE'};
% iso3s_todo_control = {'LSO'};

tic
% --- Extrapolate to fit WHO estimates and write results
for iiso3 = 1:length(iso3s_todo_control)
    clearvars -except iso3s_todo_control iiso3;
    fprintf('%0.5g: ', iiso3);
    iso3 = iso3s_todo_control{iiso3};

    Extrapolate_Countries5;
    fprintf('\n');  
end
toc





return;


% --- Simulate disruptions 
tic
for ictry = 1:length(ctrys)
    clearvars -except ctrys ictry; 
    fprintf('%0.5g: ', ictry);
    ctry = ctrys{ictry};
    Simulate_disruptions2;
    close all;
end
toc