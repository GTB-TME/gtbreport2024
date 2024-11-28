% Iterate over all countries to simulate disruptions

clear all;

opt = 'Collate countries';
% opt = 'Collate regions';


% -------------------------------------------------------------------------
% --- Pull together all modelled countries

if strcmp(opt,'Collate countries')
    % iso3s_todo = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    %     'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','RUS','THA','TLS','VNM','ZWE'};
    
    iso3s_todo = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','UKR','VNM','ZWE'};

    alltbl = {};
    % --- Get extrapolated estimates and concatenate
    for iiso3 = 1:length(iso3s_todo)
        clearvars -except iso3s_todo iiso3 alltbl;
        fprintf('%0.5g: ', iiso3);
        iso3 = iso3s_todo{iiso3};
        load([iso3, '/extrapolated_outputs2024_simple.mat'],'tbl');
        alltbl = [alltbl; tbl];
        fprintf('\n');
    end
    
    % Formulate file name
    % [yy,mm,dd] = ymd(datetime("today"));    
    % if mm < 10
    %     smm = ['0',num2str(mm)];
    % else
    %     smm = num2str(mm);
    % end
    % fname = [num2str(yy-2000), smm, num2str(dd), '_Modelled_Countries_2024.csv'];
    fname = '310730_Countries_modelled.csv';
    cell2csv(fname,alltbl);

    
    
% -------------------------------------------------------------------------
% --- Pull together all extrapolated countries in modelled regions
        
elseif strcmp(opt,'Collate regions')
    regs_todo = {'region_AFR', 'region_AMR', 'region_EUR'};
    
    
    alltbl = {};
    % --- Get extrapolated estimates and concatenate
    for ireg = 1:length(regs_todo)
        clearvars -except regs_todo ireg alltbl;
        fprintf('%0.5g: ', ireg);
        reg = regs_todo{ireg};
        
        load([reg, '/extrapolated_outputs2024.mat'],'tbl');
        alltbl = [alltbl; tbl];
        fprintf('\n');
    end
    % Formulate file name
    % [yy,mm,dd] = ymd(datetime("today"));  
    % tmp = [num2str(yy-2000), num2str(mm), num2str(dd)];
    % fname = join([num2str(yy-2000), num2str(mm), num2str(dd), "_Regionally_modelled.csv"],'');
    fname = '240702_Regional_modelled.csv';
    cell2csv(fname,alltbl);
end


return;


% -------------------------------------------------------------------------
% --- Check that csv outputs are similar to modelled ones

C = readtable('240702_Countries_modelled.csv');

iso3 = 'KGZ';
rows = find(strcmp(C.Var1,iso3));
mat  = C{rows,end-2:end};

nhiv    = length(unique(C(rows,:).Var3));
dims    = size(mat);
newdims = [nhiv, 2, 2, 5, 3];
mat2    = reshape(mat,newdims);                                            % Dims: 1.HIV status 2.Scenario 3.Incd/Mort 4.Year 5.Lo/Md/Hi
mat3    = permute(mat2,[5,4,2,3,1]);                                       % Dims: 1.Lo/Md/Hi 2.Year 3.Scenario 4.Incd/Mort 5.HIV status

extr = squeeze(mat3(:,:,:,:,1));
cols = linspecer(2);

figure;
years = 2019:2023;
for ii = 1:2
    subplot(1,2,ii);
    for jj = 1:2
        mat4 = extr(:,:,jj,ii);
        plot(years,mat4(1,:),'Color',cols(jj,:)); hold on;
        jbfill(years,mat4(3,:),mat4(2,:),cols(jj,:),'None',1,0.1); hold on;
    end
    %yl = ylim; yl(1) = 0; ylim(yl);
end
mat4i = mat4;
pause;

% Find the country-specific outputs directly from the country folder
load([iso3,'/extrapolated_outputs2024_simple.mat']);
years = 2019:2023;
for ii = 1:2
    subplot(1,2,ii);
    for jj = 1:2
        mat4 = all_pct_yr2(:,1:length(years),jj,ii);
        plot(years,mat4(2,:),'Color',cols(jj,:),'linestyle','--','LineWidth',3); hold on;
        jbfill(years,mat4(3,:),mat4(1,:),cols(jj,:),'None',1,0.1); hold on;
    end
    %yl = ylim; yl(1) = 0; ylim(yl);
end
mat4b = mat4;
