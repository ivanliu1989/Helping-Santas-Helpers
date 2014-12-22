#include "stdafx.h" 
#include <math.h> 
#include <time.h> 

using namespace std; 

const int N_ANT_COUNT = 34; //蚂蚁数量，一般取值原则为：城市数量 / 蚂蚁数量 = 1.5左右 
const int N_CITY_COUNT = 51; //城市数量 
const int N_IT_COUNT = 5000; //迭代次数，就是搜索次数 
const double DB_Q=100; //总的信息素 
const double DB_ALPHA=1; //信息素重要程度 
const double DB_BETA=3; //这个数越大，则蚂蚁往信息素大的地方走的概率就越大 
const double DB_ROU=0.5; //环境信息素挥发速度 
int besttour[N_CITY_COUNT];//最佳路径列表 

//获得指定范围内的一个随机数 
double rnd(int low,double uper) 
{ 
	double p=(rand()/(double)RAND_MAX)*((uper)-(low))+(low); 
	return (p); 
}; 

//获得指定上限范围内的一个随机数 
int rnd(int uper) 
{ 
	return (rand()%uper); 
}; 

//tsp地图信息，包含了信息素，城市距离，和信息素变化矩阵 
class GInfo 
{ 
public:  
double m_dDeltTrial[N_CITY_COUNT][N_CITY_COUNT]; //临时保存信息素，更新环境信息素的时候使用，每只蚂蚁周游完各个城市后开始计算 
double m_dTrial[N_CITY_COUNT][N_CITY_COUNT]; //城市间的信息素，就是环境信息素 
double distance[N_CITY_COUNT][N_CITY_COUNT]; //城市间距离 
}; 

GInfo Map; 

//定义蚂蚁类 
class ant 
{ 
private: 
int ChooseNextCity(); //选择下一个城市 
double prob[N_CITY_COUNT]; //临时变量数组，选择下一个城市的时候，保存各个城市被选中的概率值 
int m_nCityCount; //记录蚂蚁已经走过的城市数目 
int AllowedCity[N_CITY_COUNT];//没有走过的城市，数组索引对应城市编号 

public: 
	double m_dLength;  
	double m_dShortest; 
int tabu[N_CITY_COUNT]; //记录走过的城市，里面存的是城市的编号，数组索引表示走的顺序。 

public: 
	ant(); 
	void addcity(int city); 
	void Clear(); 
	void UpdateResult(); 
	void move(); 
	void move2last(); 
}; 

//只剩下一个城市没走过时才调用，直接移动到最后一个城市 
void ant::move2last() 
{ 
	for(int i=0;i<N_CITY_COUNT;i++) 
	{ 
		if (AllowedCity[i]==1) 
		{ 
			addcity(i); 
			break; 
		} 
	} 
} 


//清空数据，蚂蚁周游完各个城市后，要重新开始周游各个城市时调用 
void ant::Clear() 
{ 
	m_dLength=0; 
	for(int i=0; i<N_CITY_COUNT;i++) 
	{ 
		prob[i]=0; 
		AllowedCity[i]=1; 
	} 
i=tabu[N_CITY_COUNT-1]; //用最后一个城市作为出发城市 
m_nCityCount=0; 
addcity(i); 
}

//初始化 
ant::ant() 
{ 
	m_dLength=m_dShortest=0; 
	m_nCityCount=0; 

	for(int i=0;i<N_CITY_COUNT;i++) 
	{ 
		AllowedCity[i]=1; 
		prob[i]=0; 
	} 
} 


//增加一个城市到走过的城市数组中，并改变没走过的城市数组中的标志 
void ant::addcity(int city) 
{ 
 //add city to tabu; 
	tabu[m_nCityCount]=city; 
	m_nCityCount++; 
	AllowedCity[city]=0; 
} 


int ant::ChooseNextCity() 
{ 
//Update the probability of path selection 
//select a path from tabu[m_nCityCount-1] to next 
	int j=10000; 
	double temp=0.0; 
int curCity=tabu[m_nCityCount-1]; //当前走到那个城市了 

//先计算当前城市和没有走过的城市，两两之间的信息素的总和 
for (int i=0;i<N_CITY_COUNT;i++) 
{ 
	if (AllowedCity[i] == 1)  
	{ 
		temp=temp+pow((1.0/Map.distance[curCity][i]),DB_BETA)*pow((Map.m_dTrial[curCity][i]),DB_ALPHA); 
	} 
} 

//计算没有走过的城市被选中的概率 
double sel=0; 
for (i=0;i<N_CITY_COUNT;i++) 
{ 
	if (AllowedCity[i] == 1) 
	{ 
		prob[i]=pow((1.0/Map.distance[curCity][i]),DB_BETA)*pow((Map.m_dTrial[curCity][i]),DB_ALPHA)/temp; 
		sel+=prob[i]; 
	} 
	else  
	{ 
		prob[i]=0; 
	} 
} 

//下面的操作实际上就是遗传算法中的轮盘选择 
double mRate=rnd(0,sel); 
double mSelect=0; 
for ( i=0;i<N_CITY_COUNT;i++) 
{ 
	if (AllowedCity[i] == 1) 
	{ 
		mSelect+=prob[i] ; 
	} 

	if (mSelect>=mRate) 
	{ 
		j=i; 
		break; 
	} 
} 

//这种情况只有在temp=0.0的时候才会出现 
if (j == 10000) 
{ 
	for (i=0;i<N_CITY_COUNT;i++) 
	{ 
		if (AllowedCity[i] == 1) 
		{ 
			j=i; 
			break; 
		} 
	} 
} 
return j; 
}

//计算周游完城市后，走过的路径长度 
void ant::UpdateResult() 
{ 
 // Update the length of tour 
	for(int i=0;i<N_CITY_COUNT-1;i++) 
	{ 
		m_dLength+=Map.distance[tabu[i]][tabu[i+1]]; 
	} 
m_dLength+=Map.distance[tabu[N_CITY_COUNT-1]][tabu[0]]; //最后城市和开始城市间的距离 
} 


//移动到下一个城市 
void ant::move() 
{ 
 //the ant move to next town and add town ID to tabu. 
	int n=ChooseNextCity(); 
	addcity(n); 
} 

class project 
{ 
public: 
	double m_dLength; 
	ant ants[N_ANT_COUNT]; 

public: 
	project(); 
	void UpdateTrial(); 
	void initmap(); 
	void GetAnt(); 
	void StartSearch(); 
};

//更新环境信息素 
//这里采用的是 ANT-CYCLE 模式，即每只蚂蚁周游完城市后才更新 
//还有其他方式为蚂蚁每走一个城市就更新一次，经过试验表明，周游完后才更新比较好 
void project::UpdateTrial() 
{ 
//calculate the changes of trial information 
	int m=0; 
	int n=0; 
for(int i=0;i<N_ANT_COUNT;i++) //计算每只蚂蚁在两两城市间留下的信息素，蚂蚁走过的路径越短，留下的信息素数值越大 
{ 
for (int j=0;j<N_CITY_COUNT-1;j++) //计算两两城市间的信息素 
{ 
	m=ants[i].tabu[j]; 
	n =ants[i].tabu[j+1]; 
	Map.m_dDeltTrial[m][n]+=DB_Q/ants[i].m_dLength; 
	Map.m_dDeltTrial[n][m]+=DB_Q/ants[i].m_dLength; 
} 

//最后城市到开始城市间的信息素 
m=ants[i].tabu[N_CITY_COUNT-1]; 
n =ants[i].tabu[0]; 
Map.m_dDeltTrial[m][n]+=DB_Q/ants[i].m_dLength;   
Map.m_dDeltTrial[n][m]+=DB_Q/ants[i].m_dLength; 
} 

//最新的环境信息素 = 消失掉的信息素 +  新留下的信息素 
for (int i=0;i<N_CITY_COUNT;i++) 
{ 
	for (int j=0;j<N_CITY_COUNT;j++) 
	{ 
		Map.m_dTrial[i][j]=(DB_ROU*Map.m_dTrial[i][j]+Map.m_dDeltTrial[i][j] ); 
		Map.m_dDeltTrial[i][j]=0; 
	} 
} 
} 

//初始化 
void project::initmap() 
{ 
	for(int i=0;i<N_CITY_COUNT;i++) 
	{ 
		for (int j=0;j<N_CITY_COUNT;j++) 
		{ 
			Map.m_dTrial[i][j]=1; 
			Map.m_dDeltTrial[i][j]=0; 
		} 
	} 
} 

project::project() 
{ 
//initial map 
	initmap(); 
	m_dLength=10e9; 

	struct city 
	{ 
		int num; 
		int x; 
		int  y; 
	}	cc[N_CITY_COUNT]; 

//城市坐标数据来自国际通用的数据 eil51.tsp 
	int x_Ary[51]= 
	{ 
		37,49,52,20,40,21,17,31,52,51, 
		42,31,5,12,36,52,27,17,13,57, 
		62,42,16,8,7,27,30,43,58,58, 
		37,38,46,61,62,63,32,45,59,5, 
		10,21,5,30,39,32,25,25,48,56, 
		30 
	}; 

	int y_Ary[51]= 
	{ 
		52,49,64,26,30,47,63,62,33,21, 
		41,32,25,42,16,41,23,33,13,58, 
		42,57,57,52,38,68,48,67,48,27, 
		69,46,10,33,63,69,22,35,15,6, 
		17,10,64,15,10,39,32,55,28,37, 
		40 
	}; 

	for (int i=0;i<N_CITY_COUNT;i++) 
	{ 
		cc[i].x=x_Ary[i]; 
		cc[i].y=y_Ary[i]; 
		cc[i].num=i; 
	} 

//计算两两城市间距离，需要进行四舍五入取整 
//eil51.tsp的最短路径426，是按四舍五入取整后的距离算出来的。 
	for(int i=0;i<N_CITY_COUNT;i++) 
	{ 
		for (int j=0;j<N_CITY_COUNT;j++) 
		{ 
			Map.distance[i][j]=(int)(sqrt(pow((cc[i].x-cc[j].x),2)+pow((cc[i].y-cc[j].y),2))+0.5); 
		} 
	} 
}

void project::GetAnt() 
{ 
//初始化随机种子 
	srand((unsigned)time(NULL)); 

//为每只蚂蚁随机分配一个出发城市 
	int city=0; 
	for (int i=0;i<N_ANT_COUNT;i++) 
	{ 
		city=rnd(N_CITY_COUNT);  
		ants[i].addcity(city); 
	} 
} 


void project::StartSearch() 
{ 
//begin to find best solution 
int max=0;//every ant tours times 
double temp; 
int temptour[N_CITY_COUNT]; 

double dbMin=0.0; 

while (max < N_IT_COUNT) 
{   
dbMin=100000000.0; //本次叠迭中的最小路径长度 

for(int j=0;j<N_ANT_COUNT;j++)  
{  
	for (int i=0;i<N_CITY_COUNT-1;i++) 
	{ 
		ants[j].move(); 
	} 
} 

for(int j=0;j<N_ANT_COUNT;j++)  
{    
	ants[j].move2last(); 
ants[j].UpdateResult(); //计算路径总长度 
} 

//find out the best solution of the step and put it into temp 
temp=ants[0].m_dLength; 
for (int t=0;t<N_CITY_COUNT;t++) 
{ 
	temptour[t]=ants[0].tabu[t]; 
} 

for(int j=0;j<N_ANT_COUNT;j++)  
{ 
	if (temp>ants[j].m_dLength) 
	{ 
		temp=ants[j].m_dLength; 
		for (int t=0;t<N_CITY_COUNT;t++) 
		{ 
			temptour[t]=ants[j].tabu[t]; 
		} 
	} 

	if (dbMin>ants[j].m_dLength) 
	{ 
		dbMin=ants[j].m_dLength; 
	} 
} 

if (temp<m_dLength) 
{ 
	m_dLength=temp; 
	for (int t=0;t<N_CITY_COUNT;t++) 
	{ 
		besttour[t]=temptour[t]; 
	} 
} 

printf("%d : %.0f\n",max,m_dLength); 

UpdateTrial(); //全部蚂蚁遍历各个城市后，更新环境信息素 

for(int j=0;j<N_ANT_COUNT;j++)  //再搜索一次 
{ 
	ants[j].Clear();  
} 
max++; 
} 

printf("The shortest toure is : %f\n",m_dLength); 

for (int t=0;t<N_CITY_COUNT;t++) 
{ 
	printf(" %d ",besttour[t]); 
} 
} 

//主程序入口 
int main() 
{ 
	project TSP; 
	TSP.GetAnt(); 
	TSP.StartSearch(); 

	getchar(); 

	return 0; 
}