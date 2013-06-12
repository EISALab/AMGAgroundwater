function prep_N

%minaxp = [
%0   0   0   0;
%70000   7   90  12];
%minaxp = minaxp';
%minaxpn = bounds(4, -0.9, 0.9);
minaxp = [
0;
12];
minaxp = minaxp';
minaxpn = bounds(1, -0.9, 0.9);
minaxt = [0 12.0];
minaxtn = [0.0 1.0];

%center = [-0.8328	0.2081	-0.6541	-0.6365	-0.6195	0.0401]';
%center = [-0.6950    0.2831   -0.2613   -0.1752   -0.2144   -0.0539]';
train_data = load('nitr_train.dat');
test_data = load('nitr_test.dat');
all_data = load('nitr_all.dat');

%all_data = filter_25( all_data );
%ntrains = 151;
%data = load('93half.dat');

%p = all_data(:,1:4);
p = all_data(:,4);
t = all_data(:,5);
p = p';
p = normalize( p, minaxp, minaxpn );
t = t';
t = normalize( t, minaxt, minaxtn );

%p = p(:, 1:ntrains);
%t = t(:, 1:ntrains);


center = [-0.8767   -0.7458    0.2012   -0.6954]';
%center = [-0.6768   -0.6942    0.1173    0.1108]';
center = [-0.6311   -0.6366    0.3712    0.1988]';
center = [-0.7801   -0.7827   -0.2334   -0.1568]';

center = [-0.6290   -0.6346    0.3834    0.2017]';
center = [-0.7772   -0.7798   -0.2255   -0.1475]';

%center of filter_25 points.
center = [0.2742 ];
center = [-0.2036 ];

%center of all data points
center = [-0.7107];
center = [0.1019 ];
    
figure(1);
d = dist( p, center );
d = [d; t]';
d = sortrows(d);
plot( d(:,1), d(:,2), 'x' );

return;


vp = p(: , ntrains+1:size(p,2));
vt = t(: , ntrains+1:size(p,2));
pn = p(:, 1:ntrains);
tn = t(:, 1:ntrains);

%save_data( center, pn, tn, 90 );
save_data( center, vp, vt, 90 );

function [data] = filter_25( data )
for i=size(data,1):-1:1
    if( data(i,5)<2.5 )
        data(i,:) = [];
    end
end

function save_data( center, p, t, N )
%compute the distance of the points to the center
d = zeros(size(p,2),1);
for i=1:size(p,2)
    d(i,1) = distance( p(:,i), center );
    %d(i,1) = (p(6,i)+0.36)^2;
end
[sd, ix] = sort( d );

%reorder the points
p_all = p;
t_all = t;
for i=1:size(p,2)
    p_all(:,i) = p( :, ix(i) );
    t_all(:,i) = t( :, ix(i) );
end

%create the smaller cluster
data_all = [p_all; t_all];
data_1 = data_all(:,1:N);
data_1 = data_1';
save 'valid1.dat' data_1 '-ascii';
%save 'train1.dat' data_1 '-ascii';

%create the larger cluster
%sample 50 points from data_1.
ix = rand(80,1)*N + 0.5;
ix = round(ix);
ix = unique(ix);

data_2 = data_all(:, N+1:size(data_all,2));
data_2 = data_2';
for i=1:size(ix,1)
    data_2 = [data_2; data_1(ix(i),:)];
end
save 'valid2.dat' data_2 '-ascii';
%save 'train2.dat' data_2 '-ascii';


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