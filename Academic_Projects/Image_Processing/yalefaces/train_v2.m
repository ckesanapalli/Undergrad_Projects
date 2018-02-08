clear 
clc
%% Training phase

trainig_set_filename = 'filelist_train.txt' ; % Set the name of file containing names of img files to train

filelist=importdata(trainig_set_filename,'');
nof_images = length(filelist);
% width and height of images
[im_W im_H]=size(imread(char(filelist(1))));

img=[];
for i=1:nof_images
    img = [ img reshape( double( imread( char( filelist(i) ) ) ),[],1 ) ]; % Generate the big matrix
end

% Calculate the average face
avgface = mean( img,2 );
% Plot the average face
figure(3);
imagesc(reshape(avgface,[im_W im_H]));
axis image;
axis off;
colormap(gray);

% Subtract average face from each column of img
img0 = img - repmat(avgface, [1 nof_images]);

%Verify the new mean is zero
newmean = uint8(mean(img0,2));


%Computing PCA using covariance
C = img0'*img0;
[V D] = eig(C);
eigval = diag(D);
% sort eigenvalues in descending order 
eigval = eigval(end:-1:1); 
V = fliplr(V);

figure(4)
plot(eigval,'bo-');

%[xc,xci]=sort(diag(D),'descend');% largest eigenval

% Dcide number of eigenfaces to retain using 95% variance explained
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


%Plot the eigenfaces
figure(5);
j=0;
%Seleced Eigenfaces
PC = V(:,1:k95);
eigenfaces = img0*PC;

for k=1:k95
    j=j+1;
    subplot(5,10,j);
    imagesc(reshape(img0*V(:,j),[im_W,im_H]));
    axis image;
    axis off;
    colormap(gray);
end

%Calculate scores/weights of each image
%nsel = 30;
%W=[];
%for mi=1:nof_images
%    for k=1:nsel
%        W(mi,k) = sum(img0(:,mi).*(img0*V(:,xci(k))));
%    end
%end

% Calculate the weight vector
W=zeros(nof_images,k95);
for j=1:k95
    W(:,j) = img0'*(img0*V(:,j));
end
W = W';

projectimg = [];
for i=1:size(eigenfaces,2)
    temp = eigenfaces' * img0(:,i);
    projectimg = [projectimg temp];
end

%% Testing phase

testing_set_filename = 'filelist_test.txt';
test_filelist=importdata(testing_set_filename,'');
nof_test_images = length(test_filelist);
test_img=[];
for i=1:nof_test_images
    test_img = [ test_img reshape( double( imread( char( test_filelist(i) ) ) ),[],1 ) ]; % Generate the big matrix
end

test_img0 = test_img - repmat(avgface, [1 nof_test_images]);

test_W=zeros(nof_test_images,k95);
for j=1:k95
    test_W(:,j) = test_img0'*(img0*V(:,j));
end

test_W = test_W';

dist = zeros(nof_test_images, nof_images);
for i=1:nof_test_images
    for j=1:nof_images
        dist(i,j) = norm(test_W(:,i)-W(:,j));
    end
end

figure(6);
plot(dist);
