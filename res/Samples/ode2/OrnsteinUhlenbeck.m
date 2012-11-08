tBegin=0;
tEnd=1000;
dt=.01;

t=tBegin:dt:tEnd;
N=length(t);
IC=.5;
theta=1;
mu=20;
sigma=10;

y=zeros(N,1);
y(1)=IC;
for i=2:length(y)
    y(i)=y(i-1)+dt*(theta*(mu-y(i-1)))+sigma*sqrt(dt)*randn;
end

figure(1)
clf
plot(t,y)

