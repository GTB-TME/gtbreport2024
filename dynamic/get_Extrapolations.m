function [all_pct_yr_out, minerr] = get_Extrapolations(datavec, sims, xs, previous_estimates, align_inds, method)

if strcmp(method,'extrapolate')

    error = 10;
    jj = 1; minerr = error;
    all_pct_yr_out = NaN;

    while error > 0.5 && jj < 1e4
    %while error > 0.5 && jj < 100

        % -------------------------------------------------------------------------
        % --- Now sample from the data to get initial conditions
        % for iz = 1:size(alldat,1)
        %     dat = alldat(iz,:);
        [~, out] = get_distribution_fns(datavec, 'lognorm', 0);
        cdif = 1;
        while cdif>1e-2
            sam = lognrnd(out(1),out(2),size(xs,1),1);
            vec = prctile(sam,[2.5,50,97.5])./datavec-1;
            cdif = sum(vec.^2);
        end
        sam_who = sam;

        % -------------------------------------------------------------------------
        % --- Estimate regression coefficients for the model-based outcomes, based
        % only on incidence or mortality

        for ii = 1:size(sims,3)                                              % Baseline vs disruption scenario
            mat = sims(:,:,ii)'*1e5;
            X = [mat(:,1), xs];

            beta = [];
            for ic = 2:size(mat,2)
                Y = mat(:,ic);
                beta(:,ic) = mvregress(X,Y);
            end
            beta(1,1) = 1;

            % Make extrapolations
            X = [sam_who, xs];

            mat1 = X*beta;
            timeser(:,:,ii) = mat1;
            all_pct_mo(:,:,ii) = prctile(mat1,[2.5,50,97.5],1);         % Dims: 1.Lo/Md/Hi, 2.Month, 3.Baseline/disruption, 4.Inc all/Inc HIV/Mort all/Mort HIV

            % Also get annualised rates
            [~,all_pct_yr(:,:,ii)] = annualise(mat1,2,1,1);             % Dims: 1.Lo/Md/Hi, 2.Year, 3.Baseline/disruption, 4.Inc all/Inc HIV/Mort all/Mort HIV
        end
        % end

        % Compare projections for 2019 to 2022
        vec = all_pct_yr(2,1:4,2);
        % Compare with central estimates published in 2023 global TB report
        error = max(abs((1-vec(align_inds)./previous_estimates(align_inds))*100));  % <----------------
        errlist(jj) = error;

        % Show the minimum error so far, to give an idea of how well the
        % iterations are progressing
        if min(errlist) < minerr
            minerr = min(errlist);
            fprintf('%0.2g ', minerr);
            all_pct_yr_out = all_pct_yr;
        end
        jj=jj+1;
    end
    fprintf('\n');
    % keyboard;
else

    [tmp1,tmp2] = annualise(sims,1,1,2);
    tmp3 = permute(tmp2,[2,1,3]);

    sims_pct = tmp3*1e5;

    % Adjust to align with the data
    fac = datavec'./sims_pct(:,1,1)*12;
    all_pct_yr_out = sims_pct.*fac;
    minerr = 0;

    % keyboard;
end