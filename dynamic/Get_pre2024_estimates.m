clear all; C = readtable('Data/TB Estimates/TB_burden_countries_2024-06-23.csv');
colnames = C.Properties.VariableNames;

iso3s = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','IND','KAZ','KEN','KGZ','KHM','LSO','MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','THA','UKR','VNM','ZWE'};

rows = find(C.year>=2019);
C2 = C(rows,:);

% --- Incidence numbers ---------------------------------------------------
colsel = {'iso3','year','e_inc_100k','e_inc_100k_lo','e_inc_100k_hi'};
for ic = 1:length(colsel)
    colno(ic) = find(strcmp(colnames,colsel{ic}));
end

C3 = C2(:,colno);
for ii = 1:length(iso3s)
    iso3 = iso3s{ii};
    rows = find(strcmp(C3.iso3,iso3));
    incds.(iso3) = [C3.e_inc_100k_lo(rows,:), C3.e_inc_100k(rows,:), C3.e_inc_100k_hi(rows,:)];
end

% HIV +ve incidence
colsel = {'iso3','year','e_inc_tbhiv_100k','e_inc_tbhiv_100k_lo','e_inc_tbhiv_100k_hi'};
for ic = 1:length(colsel)
    colno(ic) = find(strcmp(colnames,colsel{ic}));
end

C3 = C2(:,colno);
for ii = 1:length(iso3s)
    iso3 = iso3s{ii};
    rows = find(strcmp(C3.iso3,iso3));
    incds_hiv.(iso3) = [C3.e_inc_tbhiv_100k_lo(rows,:), C3.e_inc_tbhiv_100k(rows,:), C3.e_inc_tbhiv_100k_hi(rows,:)];
end


% --- Mortality numbers ---------------------------------------------------
colsel = {'iso3','year','e_mort_100k','e_mort_100k_lo','e_mort_100k_hi'};
for ic = 1:length(colsel)
    colno(ic) = find(strcmp(colnames,colsel{ic}));
end

C3 = C2(:,colno);
for ii = 1:length(iso3s)
    iso3 = iso3s{ii};
    rows = find(strcmp(C3.iso3,iso3));
    morts.(iso3) = [C3.e_mort_100k_lo(rows,:), C3.e_mort_100k(rows,:), C3.e_mort_100k_hi(rows,:)];
end

% HIV +ve incidence
colsel = {'iso3','year','e_mort_tbhiv_100k','e_mort_tbhiv_100k_lo','e_mort_tbhiv_100k_hi'};
for ic = 1:length(colsel)
    colno(ic) = find(strcmp(colnames,colsel{ic}));
end

C3 = C2(:,colno);
for ii = 1:length(iso3s)
    iso3 = iso3s{ii};
    rows = find(strcmp(C3.iso3,iso3));
    morts_hiv.(iso3) = [C3.e_mort_tbhiv_100k_lo(rows,:), C3.e_mort_tbhiv_100k(rows,:), C3.e_mort_tbhiv_100k_hi(rows,:)];
end


save lookup_pre2024_incds incds morts incds_hiv morts_hiv;