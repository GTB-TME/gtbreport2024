% v2: Version to do all calculations of goodness-of-fit in one function
% v3: Allowing additional reductions in TB transmission during delta wave

% [pdif, sim, dat, inct, mort, inct2, mort2, noti, notq] = simulate_disruptions_fn_v3(xs, vec0, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts)

function [pdif, aux] = simulate_disruptions_fn_v3(xs, vec0, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts)

p  = prm.p; r = prm.r;
i  = ref.i; s = ref.s; xi = ref.xi;

ldown = 2020 + [4 6.5]/12;                                                 % Start and end dates of lockdown
delta = 2021 + [6 14]/12;
% delta = 2021 + [6 35]/12;

% -------------------------------------------------------------------------
% --- Construct the disruption vector
if strcmp(datafreq,'quarterly')
    tmp = repmat(vec0,3,1);
    vec = tmp(:)';
elseif strcmp(datafreq,'annual')
    tmp = repmat(vec0,12,1);
    vec = tmp(:)';
else
    vec = vec0;
end
fac = ones(1,(tend-2019)*12); fac(12+[1:length(vec)]) = vec;


% -------------------------------------------------------------------------
% --- Simulate the disruptions, iterating over the posterior density

% obj     = @(x) get_objective(x, prm, ref, sel, agg, gps, lhd, opts);
obj = @(x) get_objective2D(x, prm, ref, sel, agg, gps, lhd, opts);
odeopts = odeset('NonNegative',[1:i.nstates],'Refine',64,'AbsTol',1e-10,'RelTol',1e-10);

r0 = r; p0 = p;
inct = [];
noti = [];

mk = round(size(xs,1)/25);
for ii = 1:size(xs,1)
    if mod(ii,mk) == 0; fprintf('%0.5g ',ii/mk); end
    
    %[r,p] = alloc_parameters(xs(ii,:),r0,p0,prm,xi,opts);
    
    % Get the initial conditions
    [out, aux] = obj(xs(ii,:));
    
    try
    init = aux.soln(end,1:end-1);
    catch
        keyboard;
    end
    
    % --- Simulate in absence of disruption 
    if opts.hiv || opts.provs
        M0    = aux.M0;
        M_pu  = aux.M1;
        M_ART = aux.M2;
        geq = @(t,in) goveqs_scaleup2D(t, in, M0, M_pu, M_ART, [2000 2009; prm.ART_start 2019], i, prm, sel, agg);
    else
        M0  = aux.M0;
        M1  = aux.M1;
        geq = @(t,in) goveqs_scaleup(t, in, M0, M1, [2014 2019], i, prm, sel, agg);
    end
    
    [t0, soln0] = ode15s(geq, [2019:1/12:tend], init, odeopts);
    
    inct(:,ii,1)   = sum(diff(soln0(:,i.aux.inc),1),2);
    mort(:,ii,1)   = sum(diff(soln0(:,i.aux.mort),1),2);
    noti(:,:,ii,1) = sum(diff(soln0(:,i.aux.noti(1)),1),2);
    notq(:,:,ii,1) = sum(diff(soln0(1:3:end,i.aux.noti(1)),1),2);
    
    inct2(:,:,ii,1) = diff(soln0(:,i.aux.inc),1);
    mort2(:,:,ii,1) = diff(soln0(:,i.aux.mort),1);
%     noti2(:,:,ii,1) = diff(soln0(:,i.aux.noti(1)),1);
%     notq2(:,:,ii,1) = diff(soln0(1:3:end,i.aux.noti(1)),1);

    [t1, soln1] = ode15s(geq, [2019:1:tend], init, odeopts);
    dsol1 = diff(soln1,[],1);
    incd1(ii) = sum(dsol1(1,i.aux.inc))*1e5;
    

    % --- Now simulate disruption -----------------------------------------
    
    betared2 = 0;
    if opts.hiv || opts.provs
        Md0    = M0;     Md0.lambda    = M0.lambda*(1-betared(ii));
        Md_pu  = M_pu;   Md_pu.lambda  = M_pu.lambda*(1-betared(ii));
        Md_ART = M_ART;  Md_ART.lambda = M_ART.lambda*(1-betared(ii));
        
        Md02    = M0;     Md02.lambda    = M0.lambda*(1-betared2);
        Md_pu2  = M_pu;   Md_pu2.lambda  = M_pu.lambda*(1-betared2);
        Md_ART2 = M_ART;  Md_ART2.lambda = M_ART.lambda*(1-betared2);
        
        geq     = @(t,in) goveqs_scaleup_disruption2D(t, in, M0,   M_pu,   M_ART,   [2000 2009; prm.ART_start 2019], fac, i, prm, sel, agg);
        dgeq    = @(t,in) goveqs_scaleup_disruption2D(t, in, Md0,  Md_pu,  Md_ART,  [2000 2009; prm.ART_start 2019], fac, i, prm, sel, agg);
        dgeq2   = @(t,in) goveqs_scaleup_disruption2D(t, in, Md02, Md_pu2, Md_ART2, [2000 2009; prm.ART_start 2019], fac, i, prm, sel, agg);
        
    else
        Md0 = M0;        Md0.lambda = M0.lambda*(1-betared(ii));
        Md1 = M1;        Md1.lambda = M1.lambda*(1-betared(ii));
        
        Md02 = M0;       Md02.lambda = M0.lambda*(1-betared2);
        Md12 = M1;       Md12.lambda = M1.lambda*(1-betared2);
        
        geq   = @(t,in) goveqs_scaleup_disruption(t, in, M0,   M1,   [2014 2019], fac, i, prm, sel, agg);
        dgeq  = @(t,in) goveqs_scaleup_disruption(t, in, Md0,  Md1,  [2014 2019], fac, i, prm, sel, agg);
        dgeq2 = @(t,in) goveqs_scaleup_disruption(t, in, Md02, Md12, [2014 2019], fac, i, prm, sel, agg);
    end
    
    
    % Pre-disruption
    [ta, solna] = ode15s(geq, [2019 ldown(1)], init, odeopts);
    
    % During lockdown
    initb = solna(end,:);
    [tb, solnb] = ode15s(dgeq, [ldown(1) ldown(2)], initb, odeopts);
    
    % After lockdown
    initc = solnb(end,:);
    [tc, solnc] = ode15s(geq, [ldown(2) delta(1)], initc, odeopts);
    
    % During delta
    initd = solnc(end,:);
    [td, solnd] = ode15s(dgeq2, [delta(1) delta(2)], initd, odeopts);
    
    % After delta
    inite = solnd(end,:);
    [te, solne] = ode15s(geq, [delta(2) tend], inite, odeopts);
    
    % Collate solutions
    soln  = [solna; solnb(2:end,:); solnc(2:end,:); solnd(2:end,:); solne(2:end,:)];
    t     = [ta; tb(2:end); tc(2:end); td(2:end); te(2:end)];
    soln1 = interp1(t, soln, t0);
    
    inct(:,ii,2)   = sum(diff(soln1(:,i.aux.inc),1),2);
    mort(:,ii,2)   = sum(diff(soln1(:,i.aux.mort),1),2);
    noti(:,:,ii,2) = diff(soln1(:,i.aux.noti(1)),1);
    notq(:,:,ii,2) = diff(soln1(1:3:end,i.aux.noti(1)),1);
    
    inct2(:,:,ii,2) = diff(soln1(:,i.aux.inc),1);
    mort2(:,:,ii,2) = diff(soln1(:,i.aux.mort),1);
%     noti2(:,:,ii,2) = diff(soln1(:,i.aux.noti(1)),1);
%     notq2(:,:,ii,2) = diff(soln1(1:3:end,i.aux.noti(1)),1);
    
end


% -------------------------------------------------------------------------
% --- Finally, assess model fit 

% keyboard;

% Get the simulated notifications, at the same level of time aggregation as the
% data
if strcmp(datafreq,'monthly')
    tmp = permute(prctile(noti,[2.5,50,97.5],3)*1e5,[3,1,2,4]);            % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
    noti_sim = squeeze(tmp(2,:,:,2));
    dt = 12;
elseif strcmp(datafreq,'quarterly')
    tmp = permute(prctile(notq,[2.5,50,97.5],3)*1e5,[3,1,2,4]);            % Dims: 1.Lo/Md/Hi, 2.Quarter, 3.Pu/Pr, 4.Scenario
    noti_sim = squeeze(tmp(2,:,:,2));
    dt = 4;
elseif strcmp(datafreq,'annual')
    dims = size(noti);
    tmp1 = reshape(noti,[12, dims(1)/12,dims(2:end)]);
    tmp2 = squeeze(sum(tmp1,1));
    tmp3 = prctile(tmp2,[2.5,50,97.5],2)*1e5;
    tmp4 = permute(tmp3,[2,1,3]);
    noti_sim = squeeze(tmp4(2,:,2));                                       % Dims: 1.Lo/Md/Hi, 2.Year, 3.Pu/Pr, 4.Scenario
    dt = 1;
end

% Calculate relative difference between sim and data
noti_sim(1:dt) = [];
sim = noti_sim(length(vec0));
dat = notif_dat(length(vec0));
pdif = (1 - sim/dat);


% Auxiliary outputs
aux = [];
aux.sim   = sim;
aux.dat   = dat;
aux.inct  = inct;
aux.mort  = mort;
aux.inct2 = inct2;
aux.mort2 = mort2;
aux.noti  = noti;
aux.notq  = notq;

fprintf('\n');