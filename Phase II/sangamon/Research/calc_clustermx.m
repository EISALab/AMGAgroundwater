%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               sub functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [cluster_mx] = calc_clustermx( regnn, pts )
n_points = size( pts, 2 );
cluster_mx = [];
for i=1:n_points
    cluster_mx = [cluster_mx; calc_trustnetworks(regnn, pts(:,i)) ];
end

function [cluster] = calc_trustnetworks ( regnn, x )
nn_count = size(regnn, 1);

cluster = zeros(1, nn_count);
cluster(1) = 1;             %the first is a global network, always trust it.
for i=2:nn_count
    %note, calc_trustprob requires x be a row vector.
    cluster(i) = is_trusted( regnn(i), x' );
end

function [y] = is_trusted( reg, pt )
mins = reg.mins;
maxs = reg.maxs;
n = size(mins,2);

lA = pt > mins;
lB = pt < maxs;
if( sum(lA)+sum(lB)==2*n )
    dist = pt - reg.center;
    if( sum( dist.^2 < reg.radius.^2 )==n )
        %pt is in the boundary box. try to compute the probability.
        y = 1;
    else
        y = 0;
    end
else
    y = 0;
end