function [minaxp minaxpn minaxt minaxtn pn tn vp vt train_idx test_idx centers ] = load_sangamon
minaxp = [
0   0.2	0   0	0	0   ;
0.9 0.9	0.6	0.6	0.6	1.0 ];
minaxp = minaxp';
minaxpn = bounds(6, -0.9, 0.9);
minaxt = [0 15.0];
minaxtn = [0.0 1.0];

ntrains = 155;
data = load('93odd.dat');

%shuffle data
data(:,1)
data = orderby( data, randperm( size(data,1) ), 1 );
data(:,1)

%normalize the data
p = data(:,1:6);
t = data(:,7);
p = p';
p = normalize( p, minaxp, minaxpn );
t = t';
t = normalize( t, minaxt, minaxtn );

vp = p(: , ntrains+1:size(p,2));
vt = t(: , ntrains+1:size(p,2));
pn = p(:, 1:ntrains);
tn = t(:, 1:ntrains);

train_idx{1} = [1:ntrains]';
test_idx{1} = [1:size(vp,2)]';
centers{1} = mean(pn');

%train_idx{4} = [1:ntrains]';
%test_idx{4} = [1:size(vp,2)]';

%compute the cluster center.
alpha = 2;
cluster = [pn; alpha*tn]';
%cluster = [alpha*tn]';
[idx, c] = kmeans(cluster, 2);
[sil, h] = silhouette(cluster, idx);
%hist(idx);
%disp( 'c:' );
%disp( c );
%if( c(1,7)>c(2,7) )
if( c(1,1)>c(2,1) )
    idx = 2./idx;
    centers{3} = c(1,1:6);
    centers{2} = c(2,1:6);
else
    centers{2} = c(1,1:6);
    centers{3} = c(2,1:6);
end
    
idx1 = parse_idx( idx, 1 );
idx2 = parse_idx( idx, 2 );
train_idx{2} = idx1;
train_idx{3} = idx2;

cluster = [vp; alpha*vt]';
[idx, c] = kmeans(cluster, 2);
%[sil, h] = silhouette(cluster, idx);
%hist(idx);
%c
if( c(1,7)>c(2,7) )
    idx = 2./idx;
end
idx1 = parse_idx( idx, 1 );
idx2 = parse_idx( idx, 2 );
test_idx{2} = idx1;
test_idx{3} = idx2;

function [ids] = parse_idx( idx, group )
ids = [];
for i=1:size(idx,1)
    if( idx(i)==group )
        ids = [ids; i];
    end
end

%generate the [min max] matrix [n, 2]. n is the dimension. [:,1] is min
%values, and [:,2] is max values
function [z] = bounds( n, xmin, xmax )
z = [xmin, xmax];
for i=1:n-1
    z = [z; [xmin, xmax] ];
end

%p[n,m] is a matrix with m points, each point has n dimensions.
%minaxp[n, 2] minaxp(:,1) the minimum of p(:), minaxp(:,2) the maximum of p(:)
function [pn] = normalize( p, minaxp, minaxn )
sz = size(minaxp);
for i=1:sz(1)
    pn(i, :) = ( p(i, :)-minaxp(i, 1) )/( minaxp(i, 2)-minaxp(i, 1) ) * ( minaxn(i, 2)-minaxn(i, 1) ) + minaxn(i, 1);
end

%p[n,m] is a matrix with m points, each point has n dimensions.
%minaxp[n, 2] minaxp(:,1) the minimum of p(:), minaxp(:,2) the maximum of p(:)
function [pn] = unnormalize( p, minaxp, minaxn )
sz = size(minaxp);
for i=1:sz(1)
    pn(i, :) = ( p(i, :) -minaxn(i, 1) )/ ( minaxn(i, 2)-minaxn(i, 1) ) * ( minaxp(i, 2)-minaxp(i, 1) ) + minaxp(i, 1);
end

