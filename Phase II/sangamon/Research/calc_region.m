%calculate the region parameters from a data set (m,n);
%m is the number of points. n is the dimension of each point.
function [reg] = calc_region( data )
alpha = 2.5;

m = size(data,1);
n = size(data,2);
mins = min(data);
maxs = max(data);
means = mean(data);
stds = std(data);
radius = alpha * stds;
[mu vars] = multigaussian( data );

reg.mins = mins;
reg.maxs = maxs;
reg.means = means;
reg.center = means;
reg.stds = stds;
reg.radius = radius;
reg.vars = vars;
reg.mu = means;

disp( 'center:' );
disp( means );

