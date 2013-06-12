function [errors] = reg_nitr2( sed, modular )
minaxp = [
0   0.2	0   0	0	0   ;
0.9 0.9	0.6	0.6	0.6	1.0 ];
minaxp = minaxp';
minaxpn = bounds(6, -0.9, 0.9);
minaxt = [0 15.0];
minaxtn = [0.0 1.0];

ntrains = 155;
data = load('93odd.dat');

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

%compute the cluster center.
alpha = 1.5;
cluster = [pn; alpha*tn]';
[idx, c] = kmeans(cluster, 2);
[sil, h] = silhouette(cluster, idx);
%hist(idx);
%c
if( c(1,7)>c(2,7) )
    idx = 2./idx;
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

rand('seed', sed);
nodes = [4, 1];
regs = region_train2( nodes, train_idx, pn, tn, minaxpn, test_idx, vp, vt );
gate_w = region_train_gate( regs, pn, tn, vp, vt );

tp = region_predict( regs, vp );
err = sum( (tp-vt).^2 )/size(vt,2)

if( modular )
    tp = region_modular_predict( regs, gate_w, vp );
end

%normalized error
mse_norm = sum( (tp-vt).^2 )/size(vt,2) / var(vt');
corr_norm = corrcoef( tp', vt' );
corr_norm = corr_norm(1,2);

%unnormalize the data.
tp = unnormalize( tp, minaxt, minaxtn );
vt = unnormalize( vt, minaxt, minaxtn );

center = [-0.8966	0.2331	-0.8025	-0.7781	-0.74	0.5129]'; %c7
center = center(:,1);
%compute the distance of the points to the center
%center = [-0.8703    0.1877   -0.7609   -0.7619   -0.7297    0.0657]';
%center = [-0.6950    0.2831   -0.2613   -0.1752   -0.2144   -0.0539]';
d = zeros(size(tp,2),1);
for i=1:size(tp,2)
    d(i,1) = distance( vp(:,i), center );
end
error = (tp-vt).^2;
error = error';
error = [d, error];
y = sortrows( error, 1 );
figure(1);
plot( y(:,1), y(:,2), 'x' );

%compute the RMSE.
rmse = sqrt( sum( (tp-vt).^2 )/size(vt,2) );
disp( 'error is:' );
disp( [corr_norm mse_norm rmse] );

y = [tp', vt'];

figure(2);
plot( vt', tp', 'x' );
min(tp)
hold on;
plot( [0; 16], [0; 16], 'r' );
plot( [10; 10], [0; 16], ':' );
plot( [0; 16], [10; 10], ':' );
axis( [0 16 0 16] );
hold off;

[fp, fn, eall] = count_false( tp, vt );
[fp fn eall]

errors = [corr_norm mse_norm rmse fp fn eall];
return;

function [id] = find_if( array, x )
for i=1:size(array, 1)
    if( x<array(i) )
        id = i;
        return;
    end
end
id = size(array,1)+1;

function [ids] = parse_idx( idx, group )
ids = [];
for i=1:size(idx,1)
    if( idx(i)==group )
        ids = [ids; i];
    end
end

function [mse]=testnn( net, p, t )
tp = sim( net, p );
%calculate the MSE
sz = size( tp );
mse = sum( (t-tp).^2, 1 );
mse = sum( mse ) / sz(1) / sz(2);

function [fp, fn, e] = count_false( p, o)
n1 = 0;
n2 = 0;
n3 = 0;
for i=1:size(p,2)
    if( p(i)>10 && o(i)<10 )
        n1 = n1+1;
        n3 = n3 + 1;
    elseif( p(i)<10 && o(i)>10 )
        n2 = n2 +1;
        n3 = n3 + 1;
    end
end
fp = n1 / (n1+n2);
fn = n2 / (n1+n2);
e = 1 - n3 / size(p,2);

function [z] = SimNet( net, x, minaxp, minaxpn, minaxt, minaxtn )
pn = normalize( x, minaxp, minaxpn );
tn = sim( net, pn );
z = unnormalize( tn, minaxt, minaxtn );

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

function [d] = distance( x, y )
d = sqrt( sum( (x-y).^2 ) );

%generate the [min max] matrix [n, 2]. n is the dimension. [:,1] is min
%values, and [:,2] is max values
function [z] = bounds( n, xmin, xmax )
z = [xmin, xmax];
for i=1:n-1
    z = [z; [xmin, xmax] ];
end