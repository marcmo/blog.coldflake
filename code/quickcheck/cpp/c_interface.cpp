#include "c_interface.h"
#include "stdio.h"
#include "voltage/VoltageManager.h"
#include "voltage/AbstractCriticalVoltageListener.h"
#include "TimeoutManagerMock.h"
#include <vector>
#define PRINTF //

using namespace bios;
using namespace bsp;
using namespace common;
using namespace mock;
using namespace std;

struct UnderVoltageListener : public AbstractUnderVoltageListener
{
	UnderVoltageListener(ITimeoutManager2& tm, uint16 level, IVoltageListener::Battery b, uint32 debouncetime, uint16 hysteresis) :
		AbstractUnderVoltageListener(tm, level,b,debouncetime),
		fBattery(b),
		fUnderVoltageOccuredCount(0),
		fUnderVoltageLeftCount(0)
	{
		setHysteresis(hysteresis);
	}

	virtual void criticalVoltageEntered()
	{ 
		PRINTF("v");
		++fUnderVoltageOccuredCount;
	}

	virtual void criticalVoltageLeft()
	{
		PRINTF(";");
		++fUnderVoltageLeftCount;
	}

	IVoltageListener::Battery fBattery;
	int fUnderVoltageOccuredCount;
	int fUnderVoltageLeftCount;
};
int debounceTimeTest = 0;
class TestExecuter
{
public:
	TestExecuter() {
	    fpListeners = new ListenerList(); 
	}
	~TestExecuter()
    {
        fpListeners->erase(fpListeners->begin());
        delete fpListeners;
    }

    void addListener(int threshold, int debounceTime, int hysteresis)
    {
        // printf("added listener nr %d\n", fpListeners->size() + 1);
		UnderVoltageListener* x = new UnderVoltageListener(tmMock, threshold, IVoltageListener::BATTERY_L1, debounceTime, hysteresis);
		fpListeners->push_back(x);
		voltageManager.registerUnderVoltageListener(*x);
    }

	TimeoutManagerMock	tmMock;
	VoltageManager voltageManager;
	typedef vector<UnderVoltageListener*> ListenerList;
	ListenerList* fpListeners;


};
TestExecuter* pTestExecuter = 0L;

// implementation of FFI
void voltageEvent(int x)
{
	pTestExecuter->voltageManager.voltageChanged(x, IVoltageListener::BATTERY_L1);
}
void timeoutEvent()
{
	assert(pTestExecuter);
	pTestExecuter->tmMock.alarmExpired();
}
int listenerVoltageState(int index)
{
	assert(pTestExecuter);
	// printf("listenerVoltageState[%d](listener.size()=%d)\n",index, pTestExecuter->fpListeners->size());
	return (*(pTestExecuter->fpListeners))[index]->getVoltageState();
}
int underVoltageOccuredCount(int index)
{
	assert(pTestExecuter);
	// printf("underVoltageOccuredCount[%d]\n",index);
	return (*(pTestExecuter->fpListeners))[index]->fUnderVoltageOccuredCount;
}
int underVoltageLeftCount(int index)
{
	assert(pTestExecuter);
	// printf("underVoltageLeftCount[%d]\n",index);
	return (*(pTestExecuter->fpListeners))[index]->fUnderVoltageLeftCount;
}
void setupTestExecuter()
{
	pTestExecuter = new TestExecuter();
}
void addListener(int threshold, int debounceTime, int hysteresis)
{
	assert(pTestExecuter);
	PRINTF("add: (t=%d,d=%d,h=%d)\n", threshold, debounceTime, hysteresis);
	pTestExecuter->addListener(threshold, debounceTime, hysteresis);
}
int tearDownTestExecuter()
{
	if (pTestExecuter)
	{
		delete pTestExecuter;
		pTestExecuter = 0L;
	}
}

