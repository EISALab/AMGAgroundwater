function reg_N2( sed )
minaxp = [
0   0   0   0;
70000   7   90  12];
minaxp = minaxp';
minaxpn = bounds(4, -0.9, 0.9);
minaxt = [0 12.0];
minaxtn = [0.0 1.0];

%center = [-0.8767   -0.7458    0.2012   -0.6954]';
%center = [-0.6768   -0.6942    0.1173    0.1108]';

%center = [-0.6311   -0.6366    0.3712    0.1988]';
center = [-0.7801   -0.7827   -0.2334   -0.1568]';

%col(1)-Q, col(2)-P, col(3)-T, col(4)-N(t-1), col(5)-N(t)
train_data = load('nitr_train.dat');
test_data = load('nitr_test.dat');
all_data = load('nitr_all.dat');

%train_data = filter_25( train_data );
%test_data = filter_25( test_data );
all_data = filter_25( all_data );

%ntrains = 151;
%data = load('93half.dat');

%this is a correction of the train and test data, so that they are
%consistent with the paper.
test_data = circshift( test_data, [16, 0] );
temp = train_data(55:70,:);
train_data(55:70,:) = test_data(1:16, :);
test_data(1:16,:) = temp;

p = train_data(:,1:4);
%p = train_data(:,4);
t = train_data(:,5);
p = p';
t = t';
pn = normalize( p, minaxp, minaxpn );
tn = normalize( t, minaxt, minaxtn );

vp = test_data(:,1:4);
%vp = test_data(:,4);
vt = test_data(:,5);
vp = vp';
vt = vt';
vp = normalize( vp, minaxp, minaxpn );
vt = normalize( vt, minaxt, minaxtn );

train_idx{1} = [1:size(pn,2)]';
test_idx{1} = [1:size(vp,2)]';

%compute the cluster center.
alpha = 1.0;
cluster = [pn; alpha*tn]';
[idx, c] = kmeans(cluster, 2);
[sil, h] = silhouette(cluster, idx);
hist(idx);
c
if( c(1,5)>c(2,5) )
    idx = 2./idx;
end
idx1 = parse_idx( idx, 1 );
idx2 = parse_idx( idx, 2 );
train_idx{2} = idx1;
train_idx{3} = idx2;

cluster = [vp; alpha*vt]';
[idx, c] = kmeans(cluster, 2);
[sil, h] = silhouette(cluster, idx);
hist(idx);
c
if( c(1,5)>c(2,5) )
    idx = 2./idx;
end
idx1 = parse_idx( idx, 1 );
idx2 = parse_idx( idx, 2 );
test_idx{2} = idx1;
test_idx{3} = idx2;

rand('seed', sed);
nodes = [1, 1];
regs = region_train2( nodes, train_idx, pn, tn, minaxpn, test_idx, vp, vt );
%regs = region_train( nodes, p, t, center, minpts, minaxpn, vp, vt );

%test_data = filter_25( test_data );
vp = test_data(:,1:4);
vt = test_data(:,5);
vp = vp';
vt = vt';
vp = normalize( vp, minaxp, minaxpn );
vt = normalize( vt, minaxt, minaxtn );

tp = region_predict( regs, vp );

%compute the RMSE.
err_norm = sqrt( sum( (tp-vt).^2 )/size(vt,2) );
%disp( 'error is:' );
%disp( sqrt(err) );

%unnormalize the data.
tp = unnormalize( tp, minaxt, minaxtn );
vt = unnormalize( vt, minaxt, minaxtn );

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
err = sum( (tp-vt).^2 )/size(vt,2);
disp( 'error is:' );
disp( [err_norm, sqrt(err)] );

y = [tp', vt'];

figure(2);
plot( y );

figure(3);
plot( vt', tp', 'x' );
%min(tp)
hold on;
plot( [0; 16], [0; 16], 'r' );
plot( [10; 10], [0; 16], ':' );
plot( [0; 16], [10; 10], ':' );
axis( [0 16 0 16] );
hold off;

[fp, fn, eall] = count_false( tp, vt );
[fp fn eall]

return;

function [ids] = parse_idx( idx, group )
ids = [];
for i=1:size(idx,1)
    if( idx(i)==group )
        ids = [ids; i];
    end
end

function [data] = filter_25( data )
for i=size(data,1):-1:1
    if( data(i,5)<2.5 )
        data(i,:) = [];
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