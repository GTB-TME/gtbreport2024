% v2: Final version -- taking only incidence as a covariate, also
% projecting the baseline, and making outputs

% v3: Version to extrapolate with new model by country, also including
% separate layers for HIV when needed

% v4: Version to make annual, not monthly, estimates, and to reorder in
% same way as regional estimates

looping = 0;
if ~looping
    clearvars -except looping;
    iso3 = 'KGZ';
end

chck = [iso3,'/Model_setup_popupdated.mat'];
if isfile(chck)
    load(chck);
else
    load([iso3,'/Model_setup.mat']);
end

if strcmp(iso3,'IDN')
    load IDN/projections_post_caprecap.mat xs inct inct2 mort mort2;
elseif ismember(iso3,{'IDN','PHL','VNM','KAZ','KGZ'})
    load([iso3,'/projections_URaggregated']);
    %load([iso3,'/projections_URaggregatednew']);
else
    chck =[iso3,'/projections_smoothedvec.mat'];
    if isfile(chck)
        load(chck);
    else
        load([iso3,'/projections_raw3.mat']);
    end
end

getmorts = ~ismember(iso3, {'AZE','BRA','KAZ','UKR'});

load lookup_pre2024_incds;

plotting = 0;
printing = 1;

% -- Get the data
alldat = [data.inc_2019; data.inc_h1; data.mort_all; data.mort_H1]/12;

% --- Get the model simulations
tmp1 = squeeze(sum(inct2(:,[2,3],:,:),2));
tmp2 = squeeze(mort2(:,2,:,:));

allsim = cat(4,inct,tmp1,mort,tmp2);

prevestims = cat(3, incds.(iso3), incds_hiv.(iso3), morts.(iso3), morts_hiv.(iso3));
prevestims_mid = squeeze(prevestims(:,2,:))';                              % Dims: 1.All Incd/HIV+Incd/All mort/HIV+Mort, 2.Year

if ~opts.hiv
    alldat = alldat([1,3],:);
    allsim = allsim(:,:,:,[1,3]);
    prevestims_mid = prevestims_mid([1,3],:);
    prevestims = prevestims(:,:,[1,3]);
end
if strcmp(iso3,'PRK')
    alldat = [data.inc_2019; data.mort_H0]/12;
end

fprintf([iso3, '\n']);
minerr = [];
for ii = 1:size(alldat,1)
    fprintf('Stage %0.5g: ',ii);
    
    cond1 = opts.hiv & ismember(ii,[3,4]);
    cond2 = ~opts.hiv & ii == 2;
    cond3 = cond1 || cond2;

    if ~getmorts & cond3
        %all_pct_yr1(:,:,:,ii) = nan;
        all_pct_yr2(:,:,:,ii) = nan;
        minerr(ii)            = nan;
        fprintf('Skipping \n');
    else
        % [all_pct_yr1(:,:,:,ii), minerr(ii)] = get_Extrapolations(alldat(ii,:), allsim(:,:,:,ii), xs, prevestims_mid(ii,:), 1:4, 'extrapolate');
        all_pct_yr2(:,:,:,ii)               = get_Extrapolations(alldat(ii,:), allsim(:,:,:,ii), xs, prevestims_mid(ii,:), [], 'simple');
    end

    if max(minerr) > 0.5
        badiso = [badiso, ' ', iso3];
    end
end


% -------------------------------------------------------------------------
% --- Overwrite 2019 - 2023 with earlier estimates ------------------------

% tmp = permute(prevestims,[2,1,3]);
% all_pct_yr1b = all_pct_yr1;
% all_pct_yr1b(:,1:4,1,:) = permute(prevestims,[2,1,3]);
% all_pct_yr1b(:,1:4,2,:) = permute(prevestims,[2,1,3]);
% 
% all_pct_yr2b = all_pct_yr2;
% all_pct_yr2b(:,1:4,1,:) = permute(prevestims,[2,1,3]);
% all_pct_yr2b(:,1:4,2,:) = permute(prevestims,[2,1,3]);



% -------------------------------------------------------------------------
% --- Record deviations with previously published estimates

for ii = 1:size(alldat,1)
    %est1 = squeeze(all_pct_yr1(2,1:4,2,ii));
    est2 = squeeze(all_pct_yr2(2,1:4,2,ii));
    prve = prevestims_mid(ii,:);

    %dev1 = abs(1 - est1./prve);
    dev2 = abs(1 - est2./prve);
    %maxdev(ii,:) = [max(dev1), max(dev2)];
end



% -------------------------------------------------------------------------
% --- Print the results

if opts.hiv
    hivs  = {'a','pos'};
else
    hivs  = {'a'};
end
scenarios = {'Baseline','COVID'};
measures  = {'inc','mort'};
years     = [2019:2023];

% Reshape all_pct_yr so that it has an additional dimension for HIV status
dims = size(all_pct_yr2);
extr = reshape(all_pct_yr2, [dims(1:end-1) length(hivs) dims(end)/length(hivs)]);  % Dims: 1.Lo/Md/Hi 2.Year 3.Scenario 4.HIV status 5.Incd/Mort
extr = extr(:,1:5,:,:,:);

if printing
    tbl = {}; row = 1;
    for iy = 1:length(years)
        for im = 1:length(measures)
            for is = 1:length(scenarios)
                for ih = 1:length(hivs)
                    tbl{row,1} = iso3;
                    tbl{row,2} = scenarios{is};
                    tbl{row,3} = hivs{ih};
                    tbl{row,4} = measures{im};
                    tbl{row,5} = years(iy);

                    vals = extr(:,iy,is,ih,im)';
                    tbl{row,6} = vals(2);
                    tbl{row,7} = vals(1);
                    tbl{row,8} = vals(3);
                    row = row+1;
                end
            end
        end
    end
    %save([iso3, '/extrapolated_outputs2024.mat'], 'tbl', 'all_pct_yr1','all_pct_yr2','all_pct_yr1b','all_pct_yr2b');
    save([iso3, '/extrapolated_outputs2024_simple.mat']);
end



return;

ff=figure; lw = 1.5; fs = 14;
years = 2019:2023;

if size(alldat,1)==2
    nr = 1; nc = 2;
    tis = {'Incidence','Mortality'};
else
    nr = 2; nc = 2;
    tis = {'Incidence (all)','Incidence (HIV+ve)','Mortality (all)','Mortality (HIV+ve)'};
end

for ii = 1:size(alldat,1)
    %est1 = squeeze(all_pct_yr1(2,1:length(years),2,ii));
    est2 = squeeze(all_pct_yr2(2,1:length(years),2,ii));
    prve = prevestims_mid(ii,:);

    subplot(nr,nc,ii); hold on;
    pl(1,:) = plot(years(1:end-1),prve,'linewidth',lw);
    %pl(2,:) = plot(years,est1,'linewidth',lw);
    pl(3,:) = plot(years,est2,'linewidth',lw);
    
    yl = ylim; yl(1) = 0; ylim(yl);
    xlim([2019 2023]);
    title(tis{ii});
    set(gca,'fontsize',fs);
end
subplot(nr,nc,1); legend('Last published','Statistical with post-alignment','Simple scaling','location','SouthWest');
set(ff,'Position',[266   289   889   437]);




for ii = 1:size(alldat,1)
    est1 = squeeze(all_pct_yr1(2,1:length(years),2,ii));
    est2 = squeeze(all_pct_yr2(2,1:length(years),2,ii));
    prve = prevestims_mid(ii,:);

    dev1 = abs(1 - est1(1:end-1)./prve);
    dev2 = abs(1 - est2(1:end-1)./prve);
    maxdev(ii,:) = [max(dev1), max(dev2)];
end







% Find the projected notifications
[tmp1, tmp2] = annualise(squeeze(noti),1,1,2);
mat = squeeze(tmp2(:,2,:))*1e5;



return;


figure; 

subplot(1,2,1); hold on;
mat = squeeze(all_pct_yr(2,1:end,2,1));
y1 = incds.(iso3)(:,2)';
plot(mat); plot(y1);
yl = ylim; yl(1) = 0; ylim(yl);

subplot(1,2,2); hold on;
mat = squeeze(all_pct_yr(2,1:end,2,3));
y1 = morts.(iso3)(:,2)';
plot(mat); plot(y1);
yl = ylim; yl(1) = 0; ylim(yl);

% -------------------------------------------------------------------------
% --- Get the notifications

dims = size(noti);
tmp1 = reshape(noti,[12,72/12,dims(2:end)]);
tmp2 = squeeze(sum(tmp1));
tmp3 = prctile(tmp2,[2.5,50,97.5],2);
tmp4 = permute(tmp3,[2,1,3]);
squeeze(tmp4(2,:,:))*1e5
