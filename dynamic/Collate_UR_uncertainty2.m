% v2: Includes lines 34-36 to give additional outputs for the number of
% notifications that would be expected, for given (modelled) treatment 
% initiations - based on the levels of under-reporting being assumed. Doing 
% so helps to compare countries against the actual notifications that they 
% reported in 2024, to help understand where we are potentially
% under-estimating recovery
 
clear all; iso3 = 'KGZ';

fnames.IDN = {'00', '01'};
fnames.PHL = {'015','025'};
fnames.VNM = {'00', '01'};
% fnames.KAZ = {'01', '02'};
fnames.KAZ = {'026', '027'};
% fnames.KGZ = {'015','025'};
fnames.KGZ = {'0265','027'};
fnm = fnames.(iso3);

inct = []; inct2 = [];
mort = []; mort2 = [];
noti = []; noti2 = [];
xs2  = []; 

for gg = 1:2
    load([iso3,'/projections_raw_URbnew',fnm{gg}],'aux','xs');
    load([iso3,'/URvec']);

    inct  = cat(2,inct,aux.inct);
    inct2 = cat(3,inct2,aux.inct2);
    
    mort  = cat(2,mort,aux.mort);
    mort2 = cat(3,mort2,aux.mort2);
    
    tmp = fnm{gg}; 
    if length(tmp)==2
        tmp = [tmp, '0'];
    end
    urvec = 1 - URvec2*str2num(tmp)/100;

    tmp = squeeze(aux.noti);
    tmp(1:length(urvec),:,2) = tmp(1:length(urvec),:,2).*urvec';
    noti = cat(2,noti,tmp);

    xs2   = [xs2; xs];
end

% -------------------------------------------------------------------------
% --- Save into a form that can be used for the extrapolations

% return;

xs = xs2;
% save([iso3,'/projections_URaggregated.mat'],'inct','inct2','mort','mort2','noti','xs');

mat = annualise2(inct,1,1,2)*1e5; 
figure; plot(2019:2024,mat(:,:,2))

return;

% Also, plot the different contributions

load([iso3,'/projections_raw_URbnew',fnm{1}]);
inct1 = annualise(aux.inct,1,0);
mort1 = annualise(aux.mort,1,0);

load([iso3,'/projections_raw_URbnew',fnm{2}]);
inct2 = annualise(aux.inct,1,0);
mort2 = annualise(aux.mort,1,0);

inct = cat(2,inct1,inct2);
mort = cat(2,mort1,mort2);

inct1_pct = prctile(inct1,[2.5,50,97.5],2);
mort1_pct = prctile(mort1,[2.5,50,97.5],2);

inct2_pct = prctile(inct2,[2.5,50,97.5],2);
mort2_pct = prctile(mort2,[2.5,50,97.5],2);

inct_pct = prctile(inct,[2.5,50,97.5],2);
mort_pct = prctile(mort,[2.5,50,97.5],2);

inca = permute(cat(4,inct1_pct,inct2_pct,inct_pct),[2,1,3,4]);
mat = squeeze(inca(:,:,2,:));

figure;
cols = linspecer(3);
xx = 1:6;
for ii = 1:size(mat,3)
    plt = mat(:,:,ii)*1e5;
    lg(ii,:) = plot(xx,plt(2,:),'Color',cols(ii,:)); hold on;
    jbfill(xx,plt(1,:),plt(3,:),cols(ii,:),'None',1,0.05); hold on;
end
yl = ylim; yl(1) = 0; ylim(yl);
legend(lg,'10%','20%','Both');

% Show freq distributions for different incidence sources
figure; hold on; 
histogram(inct(end,:,2)); 
histogram(inct1(end,:,2)); 
histogram(inct2(end,:,2));

