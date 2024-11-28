% 2b: Aligning with v2b, getting additional data for incidence in 2014 - to
% help inform trends

clear all; 

C = readtable('TB_burden_countries_2022-03-11.csv');

colnames = C.Properties.VariableNames;

% --- Filter on 2019 year
rows    = find(C.year==2019);
C1      = C(rows,:);

% --- Pull out the relevant estimate data
cols_needed = {'iso3','e_pop_num', ... 
    'e_inc_100k_lo',            'e_inc_100k',            'e_inc_100k_hi', ...
    'e_inc_tbhiv_100k_lo',      'e_inc_tbhiv_100k',      'e_inc_tbhiv_100k_hi', ...
    'e_mort_exc_tbhiv_100k_lo', 'e_mort_exc_tbhiv_100k', 'e_mort_exc_tbhiv_100k_hi', ...
    'e_mort_tbhiv_100k_lo',     'e_mort_tbhiv_100k',     'e_mort_tbhiv_100k_hi', ...
    'e_mort_100k_lo',           'e_mort_100k',           'e_mort_100k_hi'};

estimcols = zeros(1,length(cols_needed));
for ii = 1:length(estimcols)
   estimcols(ii) = find(strcmp(colnames,cols_needed{ii})); 
end
C2 = C1(:,estimcols);
C2 = renamevars(C2,["e_inc_100k_lo","e_inc_100k","e_inc_100k_hi"],["e_inc_100k_lo_2019","e_inc_100k_2019","e_inc_100k_hi_2019"]);


% --- Also get 2014 incidence
rows    = find(C.year==2014);
C1      = C(rows,:);
cols_needed = {'iso3', 'e_inc_100k_lo',            'e_inc_100k',            'e_inc_100k_hi'};
estimcols = zeros(1,length(cols_needed));
for ii = 1:length(estimcols)
   estimcols(ii) = find(strcmp(colnames,cols_needed{ii})); 
end
C2b = C1(:,estimcols);
C2b = renamevars(C2b,["e_inc_100k_lo","e_inc_100k","e_inc_100k_hi"],["e_inc_100k_lo_2014","e_inc_100k_2014","e_inc_100k_hi_2014"]);

C3 = join(C2,C2b);
estims = C3;
save estim_data_2b estims;