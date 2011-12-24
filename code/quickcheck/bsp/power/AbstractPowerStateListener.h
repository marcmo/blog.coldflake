/** 
 * Contains interface every PowerStateListener has to implement.
 * @file 		AbstractPowerStateListener.h
 * @ingroup		bsp
 * 
 * (c) BMW AG
 */

#ifndef ABSTRACTPOWERSTATELISTENER_H_
#define ABSTRACTPOWERSTATELISTENER_H_

#include "commonTypes.h"
#include "util/SLinkedListSet.h"

namespace bsp
{

/**
 * @class		AbstractPowerStateListener
 *
 * Interface every PowerStateListener has to implement.
 * The PowerStateListener will be notified if a change in the voltage level beyond the
 * trigger level occurs. The trigger level has to be supplied when the listener
 * is added to the PowerManager
 */
class AbstractPowerStateListener :
	public SLinkedListSetNode<AbstractPowerStateListener>
{
public:
	AbstractPowerStateListener() :
		fTriggerVoltage(0)
	{}
	virtual ~AbstractPowerStateListener()
	{}

	/**
	 * Notifies the listener of a change in the voltage level beyond the
	 * trigger level. The trigger level has to be supplied when the listener
	 * is added to the PowerManager.
	 * @param currentVoltage current voltage in 1/100 Volts (i.e. 7.07V == 707)
	 */
	virtual void powerStateChanged(uint16 currentVoltage) = 0;
	
private:
	friend class VoltageManager;
	uint16 fTriggerVoltage;
	
	uint16 getTriggerVoltage()
	{
		return fTriggerVoltage;
	}
};

}// namespace bsp

#endif /*ABSTRACTPOWERSTATELISTENER_H_*/
