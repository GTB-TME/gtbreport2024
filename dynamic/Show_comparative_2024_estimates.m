% vb: When plotting 'Other', show only the simple extrapolations, not the
% statistical ones

clear all;

% opt = 'IDN Projections';
opt = 'Other';

if strcmp(opt, 'Other')

    iso3 = 'PHL'; load([iso3,'/extrapolated_outputs2024_simple.mat']);

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
        plot(years(1:end-1),prve,'linewidth',lw);
        %plot(years,est1,'linewidth',lw);
        plot(years,est2,'linewidth',lw);

        yl = ylim; yl(1) = 0; ylim(yl);
        xlim([2019 2023]);
        title(tis{ii});
        set(gca,'fontsize',fs);
    end
    subplot(nr,nc,1); legend('Last published','Simple scaling','location','SouthWest');
    set(ff,'Position',[266   289   889   437]);

    % figure; lw = 1.5; fs = 14;
    % years = 2019:2023;
    % 
    % if size(alldat,1)==2
    %     nr = 1; nc = 2;
    %     tis = {'Incidence','Mortality'};
    % else
    %     nr = 2; nc = 2;
    %     tis = {'Incidence (all)','Incidence (HIV+ve)','Mortality (all)','Mortality (HIV+ve)'};
    % end
    % 
    % for ii = 1:size(alldat,1)
    %     est1 = squeeze(all_pct_yr1(2,1:length(years),2,ii));
    %     est2 = squeeze(all_pct_yr2(2,1:length(years),2,ii));
    %     prve = prevestims_mid(ii,:);
    % 
    %     subplot(nr,nc,ii); hold on;
    %     plot(years,est1,'linewidth',lw);
    %     plot(years,est2,'linewidth',lw);
    %     plot(years(1:end-1),prve,'linewidth',lw);
    %     yl = ylim; yl(1) = 0; ylim(yl);
    %     xlim([2019 2023]);
    %     title(tis{ii});
    %     set(gca,'fontsize',fs);
    % end

end

if strcmp(opt, 'IDN Projections')

    iso3 = 'IDN'; load([iso3,'/extrapolated_outputs2024.mat']);
    load lookup_pre2024_incds.mat;

    mat1 = squeeze(all_pct_yr1b(:,:,2,1));
    mat2 = squeeze(all_pct_yr2(:,:,2,1));
    incd0 = incds.('IDN')';
    tmp = nan(size(mat1));
    tmp(:,1:size(incd0,2)) = incd0;
    allinc = cat(3,tmp,mat1,mat2);

    mat1 = squeeze(all_pct_yr1b(:,:,2,2));
    mat2 = squeeze(all_pct_yr2(:,:,2,2));
    mort0 = morts.('IDN')';
    tmp = nan(size(mat1));
    tmp(:,1:size(mort0,2)) = mort0;
    allmrt = cat(3,tmp,mat1,mat2);


    years = 2019:2024;
    ff=figure; hold on; lw = 1.5; fs = 14;

    % plt = incd0;
    % plot(years(1:4), plt(2,:), 'Color',cols(ii+1,:), 'LineWidth',lw);
    % jbfill(years(1:4),plt(3,:),plt(1,:),cols(ii+1,:),'None',1,0.1); hold on;
    % xlim([2019 2023]);
    % yl = ylim; yl(1) = 0; ylim(yl);
    % set(gca,'fontsize',fs);

    lgds = {'Last published','Now projected (statistical with post-alignment)','Now projected (simple scaling)','Mathieu estimate, 2023'};

    cols = linspecer(3);
    for ii = [1,1,2,3]

        inds = 1:5;
        if ii == 1
            inds = 1:4;
        end

        subplot(1,2,1); title('Incidence');
        plt = allinc(:,:,ii);
        lg(ii,:) = plot(years(inds), plt(2,inds), 'Color',cols(ii,:), 'LineWidth',lw);
        jbfill(years(inds),plt(3,inds),plt(1,inds),cols(ii,:),'None',1,0.1); hold on;
        ylim([0 460]); xlim([2019 2023]);
        legend(lg, lgds(1:ii), 'Location', 'SouthWest');
        set(gca,'fontsize',fs);

        subplot(1,2,2); title('Mortality');
        plt = allmrt(:,:,ii);
        plot(years(inds), plt(2,inds), 'Color',cols(ii,:), 'LineWidth',lw);
        jbfill(years(inds),plt(3,inds),plt(1,inds),cols(ii,:),'None',1,0.1); hold on;
        ylim([0 65]); xlim([2019 2023]);
        set(gca,'fontsize',fs);

        set(ff,'Position',[266   289   889   437]);
        drawnow;
        pause;
    end

    % Finally, overlay Mathieu's incidence estimate
    tmp = [273753191 275501339];
    epop = tmp(2)^2/tmp(1);

    mb_estim = [996 1082 1171]*1e3/epop*1e5;
    subplot(1,2,1);
    xx = 2022.75;
    lg(ii+1,:) = plot(xx,mb_estim(2),'.','MarkerSize',24,'Color','k');
    hilo = diff(mb_estim);
    errorbar(xx,mb_estim(2),hilo(1),hilo(2),'Color','k');
    legend(lg, lgds(1:ii+1), 'Location', 'SouthWest');

end