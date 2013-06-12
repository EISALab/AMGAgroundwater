function batch_sangamon2( seds )

n = size( seds, 1 );
fid1 = fopen( 'single_model.txt', 'w' );
fid2 = fopen( 'region_model.txt', 'w' );
for i=1:n
    %{
    fprintf( fid1, '%d\t', seds(i) );
    rand('seed', seds(i) );
    [errors] = reg_nitr(150);
    fprintf( fid1, '%f\t', errors );
    fprintf( fid1, '\n' );
    %}
    
    fprintf( fid2, '%d\t', seds(i) );
%    [errors] = reg_ann_sangamon( seds(i) )
    [errors] = reg_ann_modular_sangamon( seds(i) )
%    [errors] = reg_svm_modular_sangamon
    fprintf( fid2, '%f\t', errors );
    fprintf( fid2, '\n' );
end

fclose( fid1 );
fclose( fid2 );
for i=1:10
    beep;
end
