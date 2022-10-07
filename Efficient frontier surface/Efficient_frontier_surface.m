database=readtable("SP500 securities.xlsx");
covmat=cov(table2array(database(:,2:end)));
v=diag(covmat);
n=length(covmat);
covmat_inv=diag(ones(1,n))/covmat;

[V,D]=eig(corr(table2array(database(:,2:end)))-diag(ones(n,1)));
e=V(:,1);
mean(e)
EC=e;


% e=e/max(e)
% e=e/(sqrt(e'*e));
% e=(corr(table2array(database(:,2:end)))-diag(ones(n,1)))*e/D(1);
% (corr(table2array(database(:,2:end)))-diag(ones(n,1)))*e/D(1) - (corr(table2array(database(:,2:end)))-diag(ones(n,1)))*e
% size((corr(table2array(database(:,2:end)))-diag(ones(n,1)))*e/D(1))
% EC=covmat*e(:,1);EC=EC/sum(EC);
ER=mean(table2array(database(:,2:end)));ER=ER';

lambda=0:0.001:0.01;
gamma=0:0.001:0.01;
[Lambda,Gamma]=meshgrid(lambda,gamma);
X=zeros(size(Lambda));
Y=zeros(size(Lambda));
Z=zeros(size(Lambda));
for i=1:length(lambda)
    for j=1:length(lambda)
        alpha_lambda=Lambda(i,j)*ones(1,n)*covmat_inv*ER;
        alpha_gamma=-Gamma(i,j)*ones(1,n)*covmat_inv*EC;
        a1=ER'*covmat_inv*ER/sum(covmat_inv*ER);
        a2=sum(covmat_inv*ER)/sum(sum(covmat_inv));
        a3=ER'*covmat_inv*EC/sum(covmat_inv*ER);
        b1=EC'*covmat_inv*ER/sum(covmat_inv*ER);
        b2=sum(covmat_inv*EC)/sum(sum(covmat_inv));
        b3=EC'*covmat_inv*EC/sum(covmat_inv*ER);
        c=1/sum(covmat_inv*ER);
        d=a3/sum(covmat_inv*ER);
        Z(i,j)=ER'*covmat_inv*ones(n,1)+alpha_lambda*(a1-a2)+alpha_gamma*(a1-a3);
        X(i,j)=EC'*covmat_inv*ones(n,1)+alpha_lambda*(b1-b2)+alpha_gamma*(b1-b3);
        Y(i,j)=alpha_lambda^2*(a1-3*c)+alpha_gamma^2*(b3-3*c)+alpha_lambda*alpha_gamma*(d-4*c)...
            -alpha_lambda*c-alpha_gamma*c+c;
    end
end
XX=reshape(X,numel(X),1);
YY=reshape(Y,numel(X),1);
ZZ=reshape(Z,numel(X),1);
figure;
plot3(XX,YY,ZZ);
T=array2table(XX);



%% all stocks
ret=table2array(database(:,2:end));
covmat=cov(ret);
cormat=corr(ret);
v=diag(covmat);
n=length(covmat);
covmat_inv=diag(ones(1,n))/covmat;
[V,D]=eig(cormat-diag(ones(n,1)));
EC=V(:,1);
ER=mean(ret);ER=ER';

lambda=linspace(0,1000,20);
gamma=linspace(0,5,20);
[Lambda,Gamma]=meshgrid(lambda,gamma);
theta1=sum(sum(covmat_inv));
theta2=sum(covmat_inv*ER);
theta3=sum(covmat_inv*EC);
theta4=EC'*covmat_inv*ER;
theta5=ER'*covmat_inv*ER;
theta6=EC'*covmat_inv*EC;
alpha_gamma=Gamma*theta2;
alpha_lambda=-Lambda*theta3;
mu=theta2/theta1+alpha_gamma*(theta5/theta2-theta2/theta1)+alpha_lambda*(theta4/theta3-theta2/theta1);
phi=theta3/theta1+alpha_gamma*(theta4/theta2-theta3/theta1)+alpha_lambda*(theta6/theta3-theta3/theta1);
v_sq=(theta5/theta2^2-1/theta1)*alpha_gamma.^2+(theta6/theta3^2-1/theta1)*alpha_lambda.^2+ ...
    (theta4/theta2/theta3-1/theta1)*alpha_gamma.*alpha_lambda + 1/theta1;
figure;
surf(mu,phi,v_sq);hold on
scatter3(ER,EC,v);
zlim([-1,900000])

%% first 10 stocks
database=readtable("SP500 securities.xlsx");
ret=table2array(database(1:500,2:11));
covmat=cov(ret);
cormat=corr(ret);
v=diag(covmat);
n=length(covmat);
covmat_inv=diag(ones(1,n))/covmat;
[V,D]=eig(cormat-diag(ones(n,1)));
EC=V(:,1);
ER=mean(ret);ER=ER';

lambda=linspace(-100,100,20);
gamma=linspace(-1,1,20);
[Lambda,Gamma]=meshgrid(lambda,gamma);
theta1=sum(sum(covmat_inv));
theta2=sum(covmat_inv*ER);
theta3=sum(covmat_inv*EC);
theta4=EC'*covmat_inv*ER;
theta5=ER'*covmat_inv*ER;
theta6=EC'*covmat_inv*EC;
alpha_gamma=Gamma*theta2;
alpha_lambda=-Lambda*theta3;
mu=theta2/theta1+alpha_gamma*(theta5/theta2-theta2/theta1)+alpha_lambda*(theta4/theta3-theta2/theta1);
phi=theta3/theta1+alpha_gamma*(theta4/theta2-theta3/theta1)+alpha_lambda*(theta6/theta3-theta3/theta1);
v_sq=(theta5/theta2^2-1/theta1)*alpha_gamma.^2+(theta6/theta3^2-1/theta1)*alpha_lambda.^2+ ...
    (theta4/theta2/theta3-1/theta1)*alpha_gamma.*alpha_lambda + 1/theta1;
XX=reshape(mu,numel(mu),1);
YY=reshape(phi,numel(mu),1);
ZZ=reshape(v_sq,numel(mu),1);
figure;
surf(mu,phi,sqrt(v_sq));hold on
scatter3(ER,EC,sqrt(v));



