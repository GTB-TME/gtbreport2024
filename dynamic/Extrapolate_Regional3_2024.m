% v2: Version to create excel file with outputs
% v2: Version to create new outputs as required by Hazim et al
% v3_2024: Version used in 2024, where we don't use statistical approach,
% and instead just scale up the intervals and central estimates directly 

clear all;

reg = 'AFR'; dir = ['region_',reg,'/']; load([dir,'projections_raw3']);
% reg = 'AMR'; dir = ['region_',reg,'/']; load([dir,'projections_raw_URbnew01']);
% reg = 'EUR'; dir = ['region_',reg,'/']; load([dir,'projections_raw_URbnew015']);


inct  = aux.inct;
mort  = aux.mort;
inct2 = aux.inct2;
mort2 = aux.mort2;

% return;

% -------------------------------------------------------------------------
% --- Get the incidence and mortality rate ratios -------------------------

% IRR for total incidence
IRR_all = (inct(:,:,1)./inct(1,:,1))';
% IRR for HIV+ve
tmp = squeeze(sum(inct2(:,[2,3],:,:),2));
IRR_hiv = (tmp(:,:,1)./tmp(1,:,1))';
% MRR for total mortality
MRR_all = (mort(:,:,1)./mort(1,:,1))';
% MRR for HIV+ve
tmp = squeeze(sum(mort2(:,2,:,:),2));
MRR_hiv = (tmp(:,:,1)./tmp(1,:,1))';
% Bring them all together
if opts.hiv
    RRs_bsline = cat(3, IRR_all, IRR_hiv, MRR_all, MRR_hiv);
else
    RRs_bsline = cat(3, IRR_all, MRR_all);
end

% IRR for total incidence
IRR_all = (inct(:,:,2)./inct(1,:,2))';
% IRR for HIV+ve
tmp = squeeze(sum(inct2(:,[2,3],:,:),2));
IRR_hiv = (tmp(:,:,2)./tmp(1,:,2))';
% MRR for total mortality
MRR_all = (mort(:,:,2)./mort(1,:,2))';
% MRR for HIV+ve
tmp = squeeze(sum(mort2(:,2,:,:),2));
MRR_hiv = (tmp(:,:,2)./tmp(1,:,2))';
% Bring them all together
if opts.hiv
    RRs_disrupt = cat(3, IRR_all, IRR_hiv, MRR_all, MRR_hiv);
else
    RRs_disrupt = cat(3, IRR_all, MRR_all);
end

% Get alternative, annual multipliers
mat1    = inct;
mat2    = squeeze(sum(inct2(:,[2,3],:,:),2));
mat3    = mort;
mat4    = squeeze(sum(mort2(:,2,:,:),2));
matall  = cat(4,mat1,mat2,mat3,mat4);                                      % Dims: 1.Month 2.Sample 3.Scenario 4.Incd,HIV Incd,Mort,HIV mort
[~,tmp] = annualise(matall,1,1,2);
mat     = permute(tmp,[2,1,3,4])*1e5;
mulmat  = mat./mat(:,1,:,:);
if ~opts.hiv
    mulmat = mulmat(:,:,:,[1,3]);
end

load('Data/TB Estimates/estim_data_2b.mat');

if opts.hiv
    names = {'inc_2019','inc_h1','mort_all','mort_H1'};
else
    names = {'inc_2019','mort_all'};
end

tblinc = [];
tblmrt = [];

countries = ctrlist.(reg);

for ic = 1:length(countries)
    iso3 = countries{ic};
    
    % Get the data for incidence and mortality
    countryrow = estims(strcmp(estims.iso3,iso3),:);
    mat        = reshape(countryrow{1,3:end},[3,6])';
    
    % Make adjustments in case boundary estimates are same as central (e.g. for
    % MEX)
    inds = find(mat(:,1)==mat(:,2));
    mat(inds,1) = mat(inds,2)*0.95;
    
    inds = find(mat(:,2)==mat(:,3));
    mat(inds,3) = mat(inds,2)*1.05;
    
    data.inc_2019  = mat(1,:)/12;
    data.inc_h1    = mat(2,:)/12;
    data.mort_H1   = mat(4,:)/12;
    data.mort_all  = mat(5,:)/12;
    
    incmrt = [];
    
    for ni = 1:length(names)
        name = names{ni};
        
        [~, out] = get_distribution_fns(data.(name), 'lognorm', 0);
        cdif = 1; count = 0;
        while cdif>1e-3 && count<1e4   % <--- TEMPORARY: fix when finalising
            sam = lognrnd(out(1),out(2),size(RRs_bsline,1),1);
            vec = prctile(sam,[2.5,50,97.5])./data.(name)-1;
            cdif = sum(vec.^2); count = count+1;
            if count>1e4
                error(sprintf('Not providing good samples, iso3: %s',iso3));
            end
        end
        
        tmp1 = sam.*RRs_bsline(:,:,ni);
        tmp2 = sam.*RRs_disrupt(:,:,ni);
        tmp3 = cat(3,tmp1,tmp2);
        % Get annual aggregations
        dims = size(tmp3);
        tmp4 = reshape(tmp3,[dims(1), 12, dims(2)/12, dims(3)]);
        tmp5 = squeeze(sum(tmp4,2));
        % Get percentiles
        outmat_yr(:,:,:,ni,ic)  = prctile(tmp5,[2.5,50,97.5],1);           % 1.Lo/Md/Hi 2.Year 3.Baseline/COVID 4.Inc/Mrt by HIV 5.Country
    end

        %%% - Alternative approach, just scaling the central and
        %%% uncertainty estimates

        alldat = mat([1,2,5,4],:)';
        if ~opts.hiv
            alldat = mat([1,5],:)';
        end
        for ni = 1:size(alldat,2)
            outmat_yr2(:,:,:,ni,ic) = alldat(:,ni).*mulmat(:,:,:,ni);
        end


    % --- Save the projections under country names, to allow later
    % plotting: e.g. with Plot_projections_Regional
    incmrt = outmat_yr(:,:,:,:,ic);
    dimnames = {'1. Lo/Md/hi'; '2. Year'; '3. Baseline/COVID'; '4. Inc/Mort by HIV'};
    %save([dir,'country projections/inc_mrt_',iso3,'.mat'],'incmrt','dimnames');
end

show_comparative_plots = false;
if show_comparative_plots
    % comparative plots
    if size(alldat,2)==2
        nr = 1; nc = 2;
    else
        nr = 2; nc = 2;
    end
    for ic = 1:size(outmat_yr2,5)
        figure;
        % Plot what was previously published
        for ii = 1:size(outmat_yr,4)
            subplot(nr,nc,ii); hold on;
            % Plot with current statistical approach
            plot(2019:2024,outmat_yr(:,:,2,ii,ic),'r--');
            % Plot with new statistical approach
            plot(2019:2024,outmat_yr2(:,:,2,ii,ic),'b');
            yl = ylim; yl(1) = 0; ylim(yl);
        end
    end
end

% --------------------------tbltbl-----------------------------------------------
% --- Print the results

if opts.hiv
    hivs  = {'a','pos'};
else
    hivs  = {'a'};
end
scenarios = {'Baseline','COVID'};
measures  = {'inc','mort'};
years     = [2019:2023];

% Reshape outmat_yr so that is has an additional dimension for HIV status
dims = size(outmat_yr2);
tmp1 = reshape(outmat_yr2,[dims(1:3),length(hivs),dims(4)/length(hivs),dims(5)]);                 % 1.Lo/Md/Hi 2.Year 3.Baseline/COVID 4.HIV 5.Inc/Mrt 6.Country
extr = tmp1(:,1:5,:,:,:,:);

printing = 1;
if printing
    tbl = {}; row = 1;
    for ic = 1:length(ctrlist.(reg))
        for iy = 1:length(years)
            for im = 1:length(measures)
                for is = 1:length(scenarios)
                    for ih = 1:length(hivs)
                        tbl{row,1} = ctrlist.(reg){ic};
                        tbl{row,2} = scenarios{is};
                        tbl{row,3} = hivs{ih};
                        tbl{row,4} = measures{im};
                        tbl{row,5} = years(iy);
                        
                        vals = extr(:,iy,is,ih,im,ic)';
                        tbl{row,6} = vals(2);
                        tbl{row,7} = vals(1);
                        tbl{row,8} = vals(3);
                        row = row+1;
                    end
                end
            end
        end
        %save([iso3, '/extrapolated_outputs.mat'], 'tbl', 'all_pct_yr');
    end
    save(['region_',reg,'/extrapolated_outputs2024.mat'],'tbl','outmat_yr2');
end



return;
















if opts.hiv
    %    incmat_mo = squeeze(cat(1,outmat_mo(:,:,:,1,:),outmat_mo(:,:,:,2,:)));  % Dims: 1.Lo/Md/Hi, then HIV 2.Year 3.Baseline/COVID 4.Country
    %    mrtmat_mo = squeeze(cat(1,outmat_mo(:,:,:,3,:),outmat_mo(:,:,:,4,:)));
    
    incmat_yr = squeeze(cat(1,outmat_yr(:,:,:,1,:),outmat_yr(:,:,:,2,:)));  % Dims: 1.Lo/Md/Hi, then HIV 2.Year 3.Baseline/COVID 4.Country
    mrtmat_yr = squeeze(cat(1,outmat_yr(:,:,:,3,:),outmat_yr(:,:,:,4,:)));
else
    %    incmat_mo = squeeze(outmat_mo(:,:,:,1,:));
    %    mrtmat_mo = squeeze(outmat_mo(:,:,:,2,:));
    
    incmat_yr = squeeze(outmat_yr(:,:,:,1,:));
    mrtmat_yr = squeeze(outmat_yr(:,:,:,2,:));
end


incout_mo = []; mrtout_mo = [];
for iz1 = 1:size(incmat_mo,4)
    for iz2 = 1:size(incmat_mo,3)
        incout_mo = [incout_mo; incmat_mo(:,:,iz2,iz1)];
        mrtout_mo = [mrtout_mo; mrtmat_mo(:,:,iz2,iz1)];
    end
end

incout_yr = []; mrtout_yr = [];
for iz1 = 1:size(incmat_yr,4)
    for iz2 = 1:size(incmat_yr,3)
        incout_yr = [incout_yr; incmat_yr(:,:,iz2,iz1)];
        mrtout_yr = [mrtout_yr; mrtmat_yr(:,:,iz2,iz1)];
    end
end

% --- Construct the row labels

mat = repmat({'xx'}, 3*2*length(names)/2,length(ctrlist.(reg)));
mat{1,1} = reg;
col0 = mat(:);

mat = repmat({'xx'}, 3*2*length(names)/2,length(ctrlist.(reg)));
mat(1,:) = ctrlist.(reg)';
col1 = mat(:);

mat = repmat({'xx'},3*2*length(names)/2,length(ctrlist.(reg)));
mat(1,:) = {'Baseline'};
mat(3*2*length(names)/4+1,:) = {'With COVID'};
col2 = mat(:);

mat = repmat({'xx'},3*2*length(names)/2,length(ctrlist.(reg)));
if opts.hiv
    rows1 = [1,7];
    rows2 = [4,10];
    mat(rows1,:) = {'All'};
    mat(rows2,:) = {'HIV +ve'};
end
col3 = mat(:);

vec = {'lo','md','hi'}';
col4 = repmat(vec,length(col3)/3,1);

inctbl_mo = [col0, col1, col2, col3, col4, num2cell(incout_mo)];
mrttbl_mo = [col0, col1, col2, col3, col4, num2cell(mrtout_mo)];
inctbl_yr = [col0, col1, col2, col3, col4, num2cell(incout_yr)];
mrttbl_yr = [col0, col1, col2, col3, col4, num2cell(mrtout_yr)];

return;

cell2csv([dir,'/Incidence_projections_mo.csv'],inctbl_mo);
cell2csv([dir,'/Mortality_projections_mo.csv'],mrttbl_mo);
cell2csv([dir,'/Incidence_projections_yr.csv'],inctbl_yr);
cell2csv([dir,'/Mortality_projections_yr.csv'],mrttbl_yr);
