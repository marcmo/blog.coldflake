#ifndef IECUPOWERSTATECONTROLLER_H_
#define IECUPOWERSTATECONTROLLER_H_

#include "util/Delegate.h"
#include "commonTypes.h"

namespace bios
{

class IWakeupEventListener;

/**
 * This interfaces implies the following modes:
 *  - PreSleep:  startPreSleep() has been called but no call to
 *               fullPowerUp() or powerDown() yet
 *  - Sleep:     powerDown() has been called but not yet returned
 *  - PostSleep: powerDown() has returned but no call to fullPowerUp() yet
 *  - No Sleep:  either startPreSleep()/powerDown() has not been called
 *               or it has been canceled with fullPowerUp()
 *
 * Wakeup Reason Detection
 *
 * In mode "Sleep" all relevant changes at the wakeup inputs must be detected.
 * (i.e. no relevant change/edge must be lost)
 *
 * All wakeup reasons are recorded in a wakeup reason bit vector.
 * The wakeup reason vector is an "or" of all HW wakeup reasons which occur while in sleep.
 *
 * The reasons in the vector are cleared when powerDown() is called.
 *
 * The current wakeup reason vector is passed as argument to CheckWakeupDelegate (see below).
 *
 */
class IEcuPowerStateController
{
public:
	typedef common::Delegate<bool, uint32> tCheckWakeupDelegate;

	/**
	 * enter into mode "pre sleep"
	 * this is a chance to switch off some power consumers (go to reduced power mode)
	 */
	virtual void startPreSleep() = 0;

	enum PowerMode {
		POWER_SHUTDOWN = 0, // Shutdown power, don't restart
		POWER_RESTART = 1   // Shutdown and restart
	};

	/**
	 * enter into mode "sleep"
	 * during sleep the check wakeup delegate will be called on every HW wakeup event (cyclic and non-cyclic)
	 * returns when a call to the wakeup delegate returned "true"
	 *
	 * @param   mode        see enum PowerMode
	 * @param	delegate	optional, will be called on all wakeup events during sleep
	 * 						the current HW wakeup reason vector will be passed as parameter;
	 * 						in order to trigger a wakeup it must return "true"
	 *
	 * @return  HW wakeup reason vector (an "or" of all HW wakeup reasons which occured during sleep)
	 */
	virtual uint32 powerDown(uint8 mode, tCheckWakeupDelegate delegate) = 0;
	virtual uint32 powerDown(uint8 mode) = 0;

	/**
	 * when powerDown() returns, the ECU might still be in a reduced power mode ("post sleep")
	 * this call is used to switch to full power mode
	 */
	virtual void fullPowerUp() = 0;

	/*!
	 * new Interifice for  void setMonitorWakeUp(uint32 wupLine, bool aktiv);
	 */
	virtual bool setWakeupSourceMonitoring(uint32 source) = 0;
	/*!
	 * new Interifice for  void setMonitorWakeUp(uint32 wupLine, bool aktiv);
	 */
	virtual bool clearWakeupSourceMonitoring(uint32 source) = 0;

	virtual bool setWakeupDelegate (tCheckWakeupDelegate& delegate)=0;
	/*!
	 * get HW WUP lowLevel WUP
	 */
	virtual uint32 getWakeupSource(void)= 0;


};

}

#endif /* IECUPOWERSTATECONTROLLER_H_ */
