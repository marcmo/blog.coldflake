/** 
 * @file 		ICriticalVoltageListener.h
 * @ingroup		bsp
 * 
 */

#ifndef ICRITICALVOLTAGELISTENER_H_
#define ICRITICALVOLTAGELISTENER_H_

#include "util/SLinkedListSet.h"
#include "bsp/power/IVoltageListener.h"

namespace bsp
{
	
/**
 * @class		ICriticalVoltageListener
 *
 * Interface every class has to implement which needs to be notified if undervoltage occured
 */
class ICriticalVoltageListener :
	public SLinkedListSetNode<ICriticalVoltageListener>
{
public:
	/**
	 * Notifies the listener that undervoltage occured
	 */
	virtual void criticalVoltageEntered() = 0;
	virtual void voltageLessCritical(uint16 newVoltage) = 0;
	virtual void voltageMoreCritical(uint16 newVoltage) = 0;
	virtual void criticalVoltageLeft() = 0;
	virtual uint16 getNotificationVoltage() = 0;
	virtual IVoltageListener::Battery batteryToObserve() = 0;
	virtual uint16 getHysteresis() = 0;
	virtual void reset() = 0;
};
	
} // namespace bsp

#endif /*ICRITICALVOLTAGELISTENER_H_*/
