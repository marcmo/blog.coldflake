
extern "C" void voltageEvent(int);
extern "C" void timeoutEvent();
extern "C" int listenerVoltageState(int);
extern "C" int underVoltageOccuredCount(int);
extern "C" int underVoltageLeftCount(int);
extern "C" void addListener(int, int, int);
extern "C" void setupTestExecuter();
extern "C" int tearDownTestExecuter();

void voltageEvent(int);
void timeoutEvent();
int listenerVoltageState(int);
int underVoltageOccuredCount(int);
int underVoltageLeftCount(int);
void addListener(int, int, int);
void setupTestExecuter();
int tearDownTestExecuter();

