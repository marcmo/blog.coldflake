/**
 * Contains interface of a class which provides Informations about the voltage on the board.
 * @file 		IVoltageManager.h
 * @ingroup		bsp
 *
 * (c) BMW AG
 */
#ifndef IVOLTAGEMANAGERV1_H_
#define IVOLTAGEMANAGERV1_H_

#include "commonTypes.h"

namespace bsp
{
class AbstractPowerStateListener;
class IUnderVoltageListener;

/**
 * @class		IVoltageManager
 *
* Interface of a class which provides Informations about the voltage on the board.
 */
class IVoltageManager
{
public:
	/**
	 * Destructor
	 */
	virtual ~IVoltageManager() {}

	/**
	 * registers a PowerStateListener at the VoltageManager
	 * @param listener The listener to register
	 * @param triggerVoltage The trigger voltage in 1/100 Volts (i.e. 7.07V == 707)
	 **/
	virtual void addPowerStateListener(
			AbstractPowerStateListener& listener,
			uint16 triggerVoltage) = 0;

	/**
	 * removes a PowerStateListener from the VoltageManager
	 * @param listener The listener to remove
	 **/
	virtual void removePowerStateListener(
		AbstractPowerStateListener& listener) = 0;

	/**
	 * registers a UnderVoltageListener at the VoltageManager
	 * @param listener The listener to register
	 **/
	virtual void addUnderVoltageListener(IUnderVoltageListener& listener) = 0;

	/**
	 * removes a UnderVoltageListener from the VoltageManager
	 * @param listener The listener to remove
	 **/
	virtual void removeUnderVoltageListener(IUnderVoltageListener& listener) = 0;

	/**
	 * Returns the current voltage
	 * @return The current voltage in 1/100 Volts (i.e. 7.07V == 707)
	 **/
	virtual uint16 getCurrentVoltage() = 0;

	/**
	 * Returns information whether undervoltage occurred
	 * @return Is undervoltage occurred since last notification over function
	 * undervoltageOccurred()
	 */
	virtual bool getUndervoltageOccurred() = 0;
};

}

#endif /*IVOLTAGEMANAGERV1_H_*/
