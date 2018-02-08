clear 
clc
%% Training phase

trainig_set_filename = 'filelist_train.txt' ; % Set the name of file containing names of img files to train

filelist=importdata(trainig_set_filename,'');
nof_images = length(filelist);
% width and height of images
[im_W im_H]=size(imread(char(filelist(1))));
X=[];
for i=1:nof_images
    X = [ X reshape( double( imread( char( filelist(i) ) ) ),[],1 ) ]; % Generate the big matrix
end

m=mean(X,2);

A=[];
for i=1:nof_images
    temp= double(X(:,i)-m);
    A=[A temp];
end

L=A'*A;
[V D]=eig(L);

eigval = diag(D);
% sort eigenvalues in descending order 
eigval = eigval(end:-1:1); 
V = fliplr(V);
eigsum = sum(eigval); 
csum = 0; 
for i = 1:nof_images
    csum = csum + eigval(i); 
    tv = csum/eigsum; 
    if tv > 0.95 
        k95 = i; 
        break 
    end 
end

L_eig_vec = V(:,1:k95);
%L_eig_vec = [];
%%for i = 1 : size(V,2) 
%   if( D(i,i) > 1 )
%        L_eig_vec = [L_eig_vec V(:,i)];
%    end
%end

%%% finally the eigenfaces %%%
eigenfaces = A * L_eig_vec;

projectimg = [ ];  % projected image vector matrix
for i = 1 : size(eigenfaces,2)
    temp = eigenfaces' * A(:,i);
    projectimg = [projectimg temp];
end

testing_set_filename = 'filelist_test.txt';
test_filelist=importdata(testing_set_filename,'');
nof_test_images = length(test_filelist);
index_vals = [];

for i=1:nof_test_images
    test_image = imread(char( test_filelist(i) ));
    test_image = test_image(:,:,1);
    [r c] = size(test_image);
    temp = reshape(test_image',r*c,1); % creating (MxN)x1 image vector from the 2D image
    temp = double(temp)-m; % mean subtracted vector
    projtestimg = eigenfaces'*temp; % projection of test image onto the facespace

%%%%% calculating & comparing the euclidian distance of all projected trained images from the projected test image %%%%%

    euclide_dist = [ ];
    for i=1 : size(eigenfaces,2)
        temp = (norm(projtestimg-projectimg(:,i)))^2;
        euclide_dist = [euclide_dist temp];
    end
    [euclide_dist_min recognized_index] = min(euclide_dist);
    index_vals = [index_vals recognized_index];
end

index_vals = index_vals';

result = [];
for i=1:size(index_vals,1)
     result = [result  filelist(index_vals(i)) ];
end
result = result';


