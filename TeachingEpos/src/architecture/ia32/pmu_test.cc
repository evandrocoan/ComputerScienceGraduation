// EPOS Scheduler Test Program

#include <utility/ostream.h>
#include <machine.h>
#include <display.h>
#include <thread.h>
#include <semaphore.h>
#include <alarm.h>
#include <perf_mon.h>
#include <periodic_thread.h>
#include <utility/random.h>
#include <clock.h>
#include <chronometer.h>

using namespace EPOS;

#define ARRAY_SIZE 256*1024
#define MEMORY_ACCESS 16384
#define WRITE_RATIO 4
#define POLLUTE_BUFFER_SIZE 512*1024
#define APP_REPETITIONS 1

const unsigned int TASKS = 17;
const unsigned int TIMES = 5;

int job(unsigned int repetitions, int id);
int pollute_job(unsigned int repetitions, int id);
int run(int test);

OStream cout;
Chronometer chrono;
Periodic_Thread * thread[TASKS];
RTC::Microsecond wcet[TASKS];
Clock clock;
Semaphore sem;
Display d;
int *pollute_buffer;
int *array[TASKS];

void task_0();
void task_1();
void task_2();
void task_3();
void task_4();
void task_5();
void task_6();
void task_7();
void task_8();
void task_9();
void task_10();
void task_11();
void task_12();
void task_13();
void task_14();
void task_15();
void task_16();

// Task set (in us)
struct Task_Set {
    void (* f)();
    int d;
    int p;
    int c;
    int a;
	int cpu;
} set[TASKS] = {
    { &task_0, 50000 , 50000 , 32329 , RT_Thread::NOW, 0 },
    { &task_1, 50000 , 50000 , 5260 , RT_Thread::NOW, 1 },
	{ &task_2, 50000 , 50000 , 12295 , RT_Thread::NOW, 2 },
	{ &task_3, 200000 , 200000 , 62727 , RT_Thread::NOW, 3 },
	{ &task_4, 100000 , 100000 , 49286 , RT_Thread::NOW, 4 },
	{ &task_5, 200000 , 200000 , 48083 , RT_Thread::NOW, 5 },
	{ &task_6, 200000 , 200000 , 22563 , RT_Thread::NOW, 6 },
	{ &task_7, 100000 , 100000 , 17871 , RT_Thread::NOW, 7 },
	{ &task_8, 25000 , 25000 , 15211 , RT_Thread::NOW, 1 },
	{ &task_9, 200000 , 200000 , 129422 , RT_Thread::NOW, 6 },
	{ &task_10, 200000 , 200000 , 52910 , RT_Thread::NOW, 7 },
	{ &task_11, 100000 , 100000 , 14359 , RT_Thread::NOW, 5 },
	{ &task_12, 25000 , 25000 , 14812 , RT_Thread::NOW, 2 },
	{ &task_13, 50000 , 50000 , 33790 , RT_Thread::NOW, 3 },
	{ &task_14, 25000 , 25000 , 7064 , RT_Thread::NOW, 5 },
	{ &task_15, 100000 , 100000 , 20795 , RT_Thread::NOW, 7 },
	{ &task_16, 200000 , 200000 , 42753 , RT_Thread::NOW, 4 }
};

void task_0() {
    job(set[0].c / 1730, 0);
}

void task_1()
{
    job(set[1].c / 1730, 1);
}

void task_2()
{
    job(set[2].c / 1730, 2);
}

void task_3() {
    job(set[3].c / 1730, 3);
}

void task_4()
{
    job(set[4].c / 1730, 4);
}

void task_5()
{
    job(set[5].c / 1730, 5);
}

void task_6() {
    job(set[6].c / 1730, 6);
}

void task_7()
{
    job(set[7].c / 1730, 7);
}

void task_8()
{
    job(set[8].c / 1730, 8);
}

void task_9() {
    job(set[9].c / 1730, 9);
}

void task_10()
{
    job(set[10].c / 1730, 10);
}

void task_11()
{
    job(set[11].c / 1730, 11);
}

void task_12()
{
    job(set[12].c / 1730, 12);
}

void task_13()
{
    job(set[13].c / 1730, 13);
}

void task_14() {
    job(set[14].c / 1730, 14);
}

void task_15()
{
    job(set[15].c / 1730, 15);
}

void task_16()
{
    pollute_job(set[16].c / 1730, 16);
}

int main()
{
  	d.clear();  

	cout << "P-EDF application! max cpus = " << Machine::n_cpus() << "\n";

	pollute_buffer = new int[POLLUTE_BUFFER_SIZE];  

	for(int i = 0; i < TASKS; i++)
		array[i] = new int[ARRAY_SIZE];

  	for(int i = 0; i < APP_REPETITIONS; i++) {
		cout << "Starting test " << i << "\n";
 		run(i);
  	}
	  
  	cout << "P-EDF application done!\n";

	delete pollute_buffer;

	//the while(1) avoids the restarting on a real machine
	while(1);
}

int run(int test)
{
	for(unsigned int i = 0; i < 0xffffffff; i++)
		for(unsigned int j = 0; j < 0xffffffff; j++) ;
		
	for(int i = 0; i < TASKS; i++)
		    wcet[i] = 0;

	for(int i = 0; i < TASKS; i++)
		thread[i] = new RT_Thread(set[i].f, set[i].d, set[i].p, set[i].c, set[i].a, TIMES, set[i].cpu);

	chrono.start();

	for(int i = 0; i < TASKS; i++) {
		thread[i]->join();
	}

	chrono.stop();

	cout << "P-EDF test " << test << " done in " << chrono.read() / 1000000 << " seconds!\n";

	cout << "Deleting threads..";

	for(int i = 0; i < TASKS; i++) {
		delete thread[i];
	}

	cout << "done\n";

}

int pollute_job(unsigned int repetitions, int id)
{
    int sum = 0;
    Chronometer c;
    Random * rand;
    
    rand = new Random();
    
    rand->seed(clock.now());
      
	c.start();
     
	for(int j = 0; j < repetitions; j++) {
		for(int k = (rand->random() % (POLLUTE_BUFFER_SIZE - 1) ) % 1000; k < POLLUTE_BUFFER_SIZE; k += 64) {
			pollute_buffer[k] = j % 64;
			sum += pollute_buffer[k];
		}
	}
      
  	c.stop();
      
  	if(wcet[id] < c.read())
  		wcet[id] = c.read();
      
  	c.reset();
    
    delete rand;
    
    return sum;
}

int job(unsigned int repetitions, int id)
{
    int sum = 0;
    Chronometer c;
    Random * rand;
	unsigned long long llc_misses = 0;
	unsigned long long llc_hit = 0;
	//Perf_Mon perf;

	sem.p();
	cout << "Starting thread = " << id << " CPU = " << Machine::cpu_id() << "\n";
	sem.v();

    rand = new Random();
        
    rand->seed(clock.now() + id);
	  
  	//perf.llc_misses();
  	//perf.llc_hit();
  	c.reset();
      
  	c.start();
      
  	for(int j = 0; j < repetitions; j++) {
		for(int k = 0; k < MEMORY_ACCESS; k++) {
		    int pos = rand->random() % (ARRAY_SIZE - 1);            
		    sum += array[id][pos];
    		if((k % WRITE_RATIO) == 0)
	      		array[id][pos] = k + j;
		}
  	}
      
	c.stop();
      
	if(wcet[id] < c.read())
		wcet[id] = c.read();

	c.reset();

	//llc_misses = (llc_misses + perf.get_llc_misses()) / 2;
	//llc_hit = (llc_hit + perf.get_llc_hit()) / 2;

    
	delete rand;

	//sem.p();
    //cout << "LLC Misses = " << llc_misses << " LLC Hits = " << llc_hit << "\n";
	//cout << "Thread " << id << " has finished WCET = " << wcet[id] << "\n";
	//sem.v();

    return sum;
}
