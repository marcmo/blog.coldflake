/** 
 * Contains interface every class has to implement which needs to be notified if undervoltage (i.e. voltage < 5V) occured
 * within the last 100 ms
 * @file 		IUnderVoltageListener.h
 * @ingroup		bsp
 * 
 * (c) BMW AG
 */

#ifndef IUNDERVOLTAGELISTENER_H_
#define IUNDERVOLTAGELISTENER_H_

#include "util/SLinkedListSet.h"

namespace bsp
{
	
/**
 * @class		IUnderVoltageListener
 *
 * Interface every class has to implement which needs to be notified if undervoltage (i.e. voltage < 5V) occured
 * within the last 100 ms
 */
class IUnderVoltageListener :
	public SLinkedListSetNode<IUnderVoltageListener>
{
public:

	/**
	 * destructor
	 */
	virtual ~IUnderVoltageListener()
	{}

	/**
	 * Notifies the listener that undervoltage (i.e. voltage < 5V) occured
	 * within the last 100 ms
	 */
	virtual void undervoltageOccurred() = 0;
	
};
	
} // namespace bsp

#endif /*IUNDERVOLTAGELISTENER_H_*/
