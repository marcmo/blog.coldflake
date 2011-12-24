#ifndef ISLEEPABLE_H_
#define ISLEEPABLE_H_

// Life of an ISleepableDriver:
//
// Initialize
//
// Sleep
//
// [/Sleeping pooling loop]
//
// WakeUp
//


typedef enum
{
	DEFAULT_POWER_DOWN_MODE, DEFAULT_IDLE_MODE, DEFAULT_DIAGNOSTIC_MODE, DEFAULT_OPERATING_MODE
} DefaultOperationMode_t;

class ISleepableDriver
{

public:

	// Send device definetively to Sleep
	virtual void Sleep(DefaultOperationMode_t mode)=0;
	// Awake to Sleep
	virtual void WakeUp(DefaultOperationMode_t mode)=0;

	// ... a must have for everybody
	virtual void Initialize(DefaultOperationMode_t mode)=0;
};

// Life of an ISleepableActiveDriver:
//
// Initialize
//
// PrepareForSleep
// Sleep
// [Sleeping polling loop]
// RecoverFromSleep
//
// RestoreToSleep
// [/Sleeping polling loop]
//
// WakeUp
//

class ISleepableActiveDriver : public ISleepableDriver
{
public:
	// Prepare the device for a Sleeping pooling session
	virtual void PrepareForSleep(DefaultOperationMode_t mode)=0;

	// Minimal initialization of the driver during sleep mode
	virtual void RecoverFromSleep(DefaultOperationMode_t mode)=0;

	// Minimal actions to sleep again
	virtual void RestoreToSleep(DefaultOperationMode_t mode)=0;
};

#endif /* ISLEEPABLE_H_ */
